open Cpr_lib

module Make (Parameters : Tailstormll.Parameters) = struct
  module Protocol = Tailstormll.Make (Parameters)
  open Protocol

  let key = "ssz"
  let info = "SSZ'16-like attack space"

  module Tailstorm_ssz = Tailstorm_ssz.Make (Parameters)
  module Observation = Tailstorm_ssz.Observation
  module Action = Ssz_tools.Action8

  module Agent (V : View with type data = data) = struct
    open Protocol.Referee (V)
    include V
    module N = Honest (V)

    type state =
      | BetweenActions of
          { public : block (* defender's preferred block *)
          ; private_ : block (* attacker's preferred block *)
          ; mining : [ `Inclusive | `Exclusive ]
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
          ; event : [ `ProofOfWork | `Network ]
          }

    let init ~roots =
      let root = N.init ~roots in
      BetweenActions
        { private_ = root
        ; public = root
        ; pending_private_to_public_messages = []
        ; mining = `Inclusive
        }
    ;;

    let preferred (BetweenActions state) = state.private_

    let vote_filter = function
      | `Inclusive -> Fun.const true
      | `Exclusive -> N.appended_by_me
    ;;

    let puzzle_payload (BetweenActions state) =
      let vote_filter = vote_filter state.mining in
      N.puzzle_payload' ~vote_filter state.private_
    ;;

    let public_visibility x =
      match visibility x with
      | `Released | `Received -> true
      | `Withheld -> false
    ;;

    let deliver_private_to_public_messages (BetweenActions state) =
      let public =
        List.fold_left
          (fun old consider -> N.update_head ~old (last_block consider))
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

    let prepare (BeforeAction state) event =
      let public, private_, event =
        match event with
        | Append _ -> failwith "not implemented"
        | ProofOfWork x ->
          (* work on private chain *)
          state.public, last_block x, `ProofOfWork
        | Network x ->
          (* simulate defender *)
          N.update_head ~old:state.public (last_block x), state.private_, `Network
      in
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
      ; event = (state.event :> [ `Append | `Network | `ProofOfWork ])
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
                 (N.update_head ~vote_filter ~old:state.public (last_block x))
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
      and mining =
        match action with
        | Adopt_Proceed | Override_Proceed | Match_Proceed | Wait_Proceed -> `Inclusive
        | Adopt_Prolong | Override_Prolong | Match_Prolong | Wait_Prolong -> `Exclusive
      in
      BetweenActions
        { public = state.public
        ; private_
        ; pending_private_to_public_messages = share
        ; mining
        }
      |> return ~share
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
