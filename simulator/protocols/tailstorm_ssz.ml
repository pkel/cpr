open Cpr_lib

module Make (Parameters : Tailstorm.Parameters) = struct
  module Protocol = Tailstorm.Make (Parameters)
  open Protocol

  let key = "ssz"
  let info = "SSZ'16-like attack space"

  module Observation = struct
    type t =
      { public_blocks : int (** number of public blocks after common ancestor *)
      ; private_blocks : int (** number of private blocks after common ancestor *)
      ; diff_blocks : int (** private_blocks - public_blocks *)
      ; public_votes : int
            (** number of public votes confirming the public leading block *)
      ; private_votes_inclusive : int
            (** number of votes confirming the private leading block *)
      ; private_votes_exclusive : int
            (** number of private votes confirming the private leading block *)
      ; public_depth : int (** public vote tree depth *)
      ; private_depth_inclusive : int
            (** private vote tree depth, including public votes *)
      ; private_depth_exclusive : int
            (** private vote tree depth, excluding private votes *)
      ; event : [ `Append | `ProofOfWork | `Network ] (* What is currently going on? *)
      }
    [@@deriving fields]

    module Normalizers = struct
      open Parameters
      open Ssz_tools.NormalizeObs

      let public_blocks = UnboundedInt { non_negative = true; scale = 1 }
      let private_blocks = UnboundedInt { non_negative = true; scale = 1 }
      let diff_blocks = UnboundedInt { non_negative = false; scale = 1 }
      let public_votes = UnboundedInt { non_negative = true; scale = k }
      let private_votes_inclusive = UnboundedInt { non_negative = true; scale = k }
      let private_votes_exclusive = UnboundedInt { non_negative = true; scale = k }
      let public_depth = UnboundedInt { non_negative = true; scale = k }
      let private_depth_inclusive = UnboundedInt { non_negative = true; scale = k }
      let private_depth_exclusive = UnboundedInt { non_negative = true; scale = k }
      let event = Discrete [ `Append; `ProofOfWork; `Network ]
    end

    let length = List.length Fields.names

    let to_floatarray t =
      let a = Float.Array.make length Float.nan in
      let set spec i field =
        Float.Array.set
          a
          i
          (Fieldslib.Field.get field t |> Ssz_tools.NormalizeObs.to_float spec);
        i + 1
      in
      let _ =
        let open Normalizers in
        Fields.fold
          ~init:0
          ~public_blocks:(set public_blocks)
          ~private_blocks:(set private_blocks)
          ~diff_blocks:(set diff_blocks)
          ~public_votes:(set public_votes)
          ~private_votes_inclusive:(set private_votes_inclusive)
          ~private_votes_exclusive:(set private_votes_exclusive)
          ~public_depth:(set public_depth)
          ~private_depth_inclusive:(set private_depth_inclusive)
          ~private_depth_exclusive:(set private_depth_exclusive)
          ~event:(set event)
      in
      a
    ;;

    let of_floatarray =
      let get spec _ i =
        (fun a -> Float.Array.get a i |> Ssz_tools.NormalizeObs.of_float spec), i + 1
      in
      let open Normalizers in
      fst
        (Fields.make_creator
           0
           ~public_blocks:(get public_blocks)
           ~private_blocks:(get private_blocks)
           ~diff_blocks:(get diff_blocks)
           ~public_votes:(get public_votes)
           ~private_votes_inclusive:(get private_votes_inclusive)
           ~private_votes_exclusive:(get private_votes_exclusive)
           ~public_depth:(get public_depth)
           ~private_depth_inclusive:(get private_depth_inclusive)
           ~private_depth_exclusive:(get private_depth_exclusive)
           ~event:(get event))
    ;;

    let to_string t =
      let conv to_s field =
        Printf.sprintf
          "%s: %s"
          (Fieldslib.Field.name field)
          (to_s (Fieldslib.Field.get field t))
      in
      let int = conv string_of_int in
      let event = conv Ssz_tools.event_to_string in
      Fields.to_list
        ~public_blocks:int
        ~private_blocks:int
        ~diff_blocks:int
        ~public_votes:int
        ~private_votes_inclusive:int
        ~private_votes_exclusive:int
        ~public_depth:int
        ~private_depth_inclusive:int
        ~private_depth_exclusive:int
        ~event
      |> String.concat "\n"
    ;;
  end

  module Action = Ssz_tools.Action8

  module Agent (V : View with type data = data) = struct
    open Protocol.Referee (V)
    include V
    module N = Honest (V)

    type state =
      | BetweenActions of
          { public : block (* defender's preferred block *)
          ; private_ : block (* attacker's preferred block *)
          ; pending_private_to_public_messages :
              block list (* messages sent with last action *)
          }

    type state_before_action =
      | BeforeAction of
          { public : block
          ; private_ : block
          }

    type observable_state =
      | Observable of
          { public : block
          ; private_ : block
          ; common : block
          ; event : [ `Append | `ProofOfWork | `Network ] * block
          }

    let init ~roots =
      let root = N.init ~roots in
      BetweenActions
        { private_ = root; public = root; pending_private_to_public_messages = [] }
    ;;

    let preferred (BetweenActions state) = state.private_

    let vote_filter = function
      | `Inclusive -> Fun.const true
      | `Exclusive -> N.appended_by_me
    ;;

    let puzzle_payload (BetweenActions state) = N.puzzle_payload state.private_

    let public_visibility x =
      match visibility x with
      | `Released | `Received -> true
      | `Withheld -> false
    ;;

    let deliver_private_to_public_messages (BetweenActions state) =
      let public =
        let vote_filter = public_visibility in
        List.fold_left
          (fun old consider -> N.update_head ~vote_filter ~old (last_summary consider))
          state.public
          state.pending_private_to_public_messages
      in
      BeforeAction { public; private_ = state.private_ }
    ;;

    module Dagtools = Dagtools.Make (struct
      include V

      type vertex = block

      let eq = block_eq
      let neq = block_neq
    end)

    (* A few things happen unconditionally, before the agent observes the state and
       chooses an action: *)
    let prepare (BeforeAction state) event =
      let public, private_, event, fresh =
        match event with
        | Append x ->
          (* The attacker updates preference if he learns new summaries. We make sure
             elsewhere that the attacker only appends summaries onto his preferred chain.
             Summaries on the public chain are received from the network, not appended
             locally *)
          assert (is_summary x);
          let private_ = N.update_head ~old:state.private_ x in
          state.public, private_, `Append, x
        | ProofOfWork x ->
          (* The attacker votes on his preferred chain. New votes do affect preferred
             summaries *)
          state.public, state.private_, `ProofOfWork, x
        | Network x ->
          (* The attacker receives votes and summaries from the network. The received
             blocks are public knowledge. The attacker emulates what the defender would do
             and updates the public chain. He does not (yet) take action on the private
             chain. *)
          let public =
            let vote_filter = public_visibility in
            if is_summary x
            then N.update_head ~vote_filter ~old:state.public x
            else (
              let s = last_summary x in
              N.update_head ~vote_filter ~old:state.public s)
          in
          public, state.private_, `Network, x
      in
      let event = event, fresh in
      let common = Dagtools.common_ancestor public private_ |> Option.get in
      Observable { public; private_; common; event }
    ;;

    let prepare state = deliver_private_to_public_messages state |> prepare

    let observe (Observable state) =
      let open Observation in
      let public_depth, public_votes =
        confirming_votes state.public
        |> List.filter public_visibility
        |> List.fold_left (fun (d, n) x -> max d (depth x), n + 1) (0, 0)
      and private_depth_inclusive, private_votes_inclusive =
        confirming_votes state.private_
        |> List.fold_left (fun (d, n) x -> max d (depth x), n + 1) (0, 0)
      and private_depth_exclusive, private_votes_exclusive =
        confirming_votes state.private_
        |> List.filter N.appended_by_me
        |> List.fold_left (fun (d, n) x -> max d (depth x), n + 1) (0, 0)
      in
      let ca_height = height state.common
      and private_height = height state.private_
      and public_height = height state.public in
      { private_blocks = private_height - ca_height
      ; public_blocks = public_height - ca_height
      ; diff_blocks = private_height - public_height
      ; public_votes
      ; private_votes_inclusive
      ; private_votes_exclusive
      ; public_depth
      ; private_depth_inclusive
      ; private_depth_exclusive
      ; event = fst state.event
      }
    ;;

    let apply (Observable state) action =
      let release kind =
        let module BlockSet =
          Set.Make (struct
            type t = block

            let compare = Compare.by compare_key key
          end)
        in
        let rec h release_now seq =
          match seq () with
          | Seq.Nil -> release_now (* override/match not possible; release all *)
          | Seq.Cons (x, seq) ->
            let release_now' = BlockSet.add x release_now in
            let vote_filter x = public_visibility x || BlockSet.mem x release_now' in
            if block_neq
                 state.public
                 (N.update_head ~vote_filter ~old:state.public (last_summary x))
            then (
              (* release_now' is just enough to override; release_now was not enough; *)
              match kind with
              | `Override -> release_now'
              | `Match -> release_now)
            else h release_now' seq
        in
        Dagtools.iterate_descendants ~include_start:true [ state.common ]
        |> Seq.filter (fun x -> not (public_visibility x))
        |> h BlockSet.empty
        |> BlockSet.elements
      in
      let share, private_ =
        match (action : Action.t) with
        | Adopt_Proceed | Adopt_Prolong -> [], state.public
        | Match_Proceed | Match_Prolong -> release `Match, state.private_
        | Override_Proceed | Override_Prolong -> release `Override, state.private_
        | Wait_Proceed | Wait_Prolong -> [], state.private_
      in
      let append =
        (* Do: replace private tip of chain with better summary, either of same height or
           of higher height.

           Don't do: append to the public tip of chain if it differs from private tip.
           This would result in attacker always adopting public chain if longer. (see
           [prepare])

           Hope I got this right! *)
        let vote_filter =
          match action with
          | Adopt_Proceed | Override_Proceed | Match_Proceed | Wait_Proceed ->
            vote_filter `Inclusive
          | Adopt_Prolong | Override_Prolong | Match_Prolong | Wait_Prolong ->
            vote_filter `Exclusive
        and extend =
          if children state.private_ = []
          then
            (* replacing state.private_ is feasible, advancing is not *)
            parents state.private_ |> List.hd |> last_summary
          else (* advancing state.private_ is not feasible, replacing is *)
            state.private_
        in
        match N.next_summary' ~vote_filter extend with
        | Some summary -> [ summary ]
        | None -> []
      in
      BetweenActions
        { public = state.public; private_; pending_private_to_public_messages = share }
      |> return ~append ~share
    ;;
  end

  let attacker (type a) policy ((module V) : (a, data) view) : (a, data) node =
    Node
      (module struct
        include Agent (V)

        let handler s e =
          let s = prepare s e in
          observe s |> policy |> apply s
        ;;
      end)
  ;;

  module Policies = struct
    let honest o =
      let open Observation in
      let open Action in
      if o.public_blocks > 0 then Adopt_Proceed else Override_Proceed
    ;;

    let release_block o =
      let open Observation in
      let open Action in
      if o.private_blocks < o.public_blocks
      then Adopt_Proceed
      else if o.private_blocks > o.public_blocks
      then Override_Proceed
      else Wait_Proceed
    ;;

    let override_block o =
      let open Observation in
      let open Action in
      if o.private_blocks < o.public_blocks
      then Adopt_Proceed
      else if o.public_blocks = 0
      then Wait_Proceed
      else Override_Proceed
    ;;

    let override_catchup o =
      let open Observation in
      let open Action in
      if o.private_blocks < o.public_blocks
      then Adopt_Proceed
      else if o.private_blocks = 0 && o.public_blocks = 0
      then Wait_Proceed
      else if o.public_blocks = 0
      then Wait_Proceed
      else if o.private_depth_inclusive = 0 && o.private_blocks = o.public_blocks + 1
      then Override_Proceed
      else if o.public_blocks = o.private_blocks
              && o.private_votes_inclusive = o.public_votes + 1
      then Override_Proceed
      else if o.private_blocks - o.public_blocks > 10
              (* fork can become really deep for strong attackers. Cut-off shortens time
                 spent in common ancestor computation. *)
      then Override_Proceed
      else Wait_Proceed
    ;;
  end

  let policies =
    let open Collection in
    let open Policies in
    empty
    |> add ~info:"emulate honest behaviour" "honest" honest
    |> add ~info:"release private block a.s.a.p." "release-block" release_block
    |> add ~info:"override public block a.s.a.p." "override-block" override_block
    |> add
         ~info:"override public head just before defender catches up"
         "override-catchup"
         override_catchup
  ;;
end
