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
      ; event : int
      }
    [@@deriving fields]

    let length = List.length Fields.names

    let low =
      { public_blocks = 0
      ; private_blocks = 0
      ; diff_blocks = min_int
      ; public_votes = 0
      ; private_votes_inclusive = 0
      ; private_votes_exclusive = 0
      ; public_depth = 0
      ; private_depth_inclusive = 0
      ; private_depth_exclusive = 0
      ; event = Ssz_tools.Event.low
      }
    ;;

    let high =
      { public_blocks = max_int
      ; private_blocks = max_int
      ; diff_blocks = max_int
      ; public_votes = max_int
      ; private_votes_inclusive = max_int
      ; private_votes_exclusive = max_int
      ; public_depth = max_int
      ; private_depth_inclusive = max_int
      ; private_depth_exclusive = max_int
      ; event = Ssz_tools.Event.high
      }
    ;;

    let to_floatarray t =
      let a = Float.Array.make length Float.nan in
      let set conv i field =
        Float.Array.set a i (Fieldslib.Field.get field t |> conv);
        i + 1
      in
      let int = set float_of_int in
      let _ =
        Fields.fold
          ~init:0
          ~public_blocks:int
          ~private_blocks:int
          ~diff_blocks:int
          ~public_votes:int
          ~private_votes_inclusive:int
          ~private_votes_exclusive:int
          ~public_depth:int
          ~private_depth_inclusive:int
          ~private_depth_exclusive:int
          ~event:int
      in
      a
    ;;

    let of_floatarray =
      let get conv _ i = (fun a -> Float.Array.get a i |> conv), i + 1 in
      let int = get int_of_float in
      fst
        (Fields.make_creator
           0
           ~public_blocks:int
           ~private_blocks:int
           ~diff_blocks:int
           ~public_votes:int
           ~private_votes_inclusive:int
           ~private_votes_exclusive:int
           ~public_depth:int
           ~private_depth_inclusive:int
           ~private_depth_exclusive:int
           ~event:int)
    ;;

    let to_string t =
      let conv to_s field =
        Printf.sprintf
          "%s: %s"
          (Fieldslib.Field.name field)
          (to_s (Fieldslib.Field.get field t))
      in
      let int = conv string_of_int in
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
        ~event:int
      |> String.concat "\n"
    ;;

    let%test _ =
      let run _i =
        let t =
          { public_blocks = Random.bits ()
          ; private_blocks = Random.bits ()
          ; diff_blocks = Random.bits ()
          ; public_votes = Random.bits ()
          ; private_votes_inclusive = Random.bits ()
          ; private_votes_exclusive = Random.bits ()
          ; public_depth = Random.bits ()
          ; private_depth_inclusive = Random.bits ()
          ; private_depth_exclusive = Random.bits ()
          ; event = Random.bits ()
          }
        in
        t = (to_floatarray t |> of_floatarray)
      in
      List.init 50 run |> List.for_all (fun x -> x)
    ;;
  end

  module Action = Ssz_tools.Action8

  module Agent (V : LocalView with type data = data) = struct
    open Protocol.Referee (V)
    include V
    module N = Honest (V)

    type state =
      | BetweenActions of
          { public : env Dag.vertex (* defender's preferred block *)
          ; private_ : env Dag.vertex (* attacker's preferred block *)
          ; pending_private_to_public_messages :
              env Dag.vertex list (* messages sent with last action *)
          }

    type state_before_action =
      | BeforeAction of
          { public : env Dag.vertex
          ; private_ : env Dag.vertex
          }

    type observable_state =
      | Observable of
          { public : env Dag.vertex
          ; private_ : env Dag.vertex
          ; common : env Dag.vertex
          ; event : [ `Append | `ProofOfWork | `Network ]
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

    let prepare (BeforeAction state) event =
      let public, private_, event =
        match event with
        | Append x -> state.public, x, `Append
        | ProofOfWork _x -> state.public, state.private_, `ProofOfWork
        | Network x ->
          let b = last_summary x
          and vote_filter = public_visibility in
          N.update_head ~vote_filter ~old:state.public b, state.private_, `Network
      in
      let common = Dag.common_ancestor view public private_ |> Option.get in
      Observable { public; private_; common; event }
    ;;

    let prepare state = deliver_private_to_public_messages state |> prepare

    let observe (Observable state) =
      let open Observation in
      let public_depth, public_votes =
        Dag.iterate_descendants votes_only [ state.public ]
        |> Seq.filter public_visibility
        |> Seq.fold_left (fun (d, n) x -> max d (depth x), n + 1) (0, 0)
      and private_depth_inclusive, private_votes_inclusive =
        Dag.iterate_descendants votes_only [ state.private_ ]
        |> Seq.fold_left (fun (d, n) x -> max d (depth x), n + 1) (0, 0)
      and private_depth_exclusive, private_votes_exclusive =
        Dag.iterate_descendants votes_only [ state.private_ ]
        |> Seq.filter N.appended_by_me
        |> Seq.fold_left (fun (d, n) x -> max d (depth x), n + 1) (0, 0)
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
      ; event = Ssz_tools.Event.to_int state.event
      }
    ;;

    let apply (Observable state) action =
      let release kind =
        let module Map = Map.Make (Int) in
        let rec h release_now seq =
          match seq () with
          | Seq.Nil -> release_now (* override/match not possible; release all *)
          | Seq.Cons (x, seq) ->
            let release_now' = Map.add (Dag.id x) x release_now in
            let vote_filter x = public_visibility x || Map.mem (Dag.id x) release_now' in
            if state.public $!= N.update_head ~vote_filter ~old:state.public x
            then (
              (* release_now' is just enough to override; release_now was not enough; *)
              match kind with
              | `Override -> release_now'
              | `Match -> release_now)
            else h release_now' seq
        in
        Dag.iterate_descendants view [ state.common ]
        |> Seq.filter (fun x -> not (public_visibility x))
        |> h Map.empty
        |> Map.bindings
        |> List.map snd
      in
      let share, private_ =
        match (action : Action.t) with
        | Adopt_Proceed | Adopt_Prolong -> [], state.public
        | Match_Proceed | Match_Prolong -> release `Match, state.private_
        | Override_Proceed | Override_Prolong -> release `Override, state.private_
        | Wait_Proceed | Wait_Prolong -> [], state.private_
      in
      let append =
        let vote_filter =
          match action with
          | Adopt_Proceed | Override_Proceed | Match_Proceed | Wait_Proceed ->
            vote_filter `Inclusive
          | Adopt_Prolong | Override_Prolong | Match_Prolong | Wait_Prolong ->
            vote_filter `Exclusive
        in
        match N.next_summary' ~vote_filter private_ with
        | Some summary -> [ summary ]
        | None -> []
      in
      BetweenActions
        { public = state.public; private_; pending_private_to_public_messages = share }
      |> return ~append ~share
    ;;
  end

  let attacker (type a) policy ((module V) : (a, data) local_view) : (a, data) node =
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
      else if o.private_blocks = 0 && o.public_blocks = 0
      then Wait_Proceed
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
