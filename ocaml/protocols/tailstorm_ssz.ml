open Cpr_lib

module Make (Parameters : Tailstorm.Parameters) = struct
  module Protocol = Tailstorm.Make (Parameters)
  open Protocol

  let key = "ssz"
  let info = "SSZ'16-like attack space"

  module Observation = struct
    type t =
      { public_blocks : int (** number of blocks after common ancestor *)
      ; public_votes : int (** number of votes confirming the public leading block *)
      ; public_depth : int (** public vote tree depth *)
      ; private_blocks : int (** number of blocks after common ancestor *)
      ; private_votes : int (** number of votes confirming the private leading block *)
      ; private_depth : int (** private vote tree depth *)
      ; diff_blocks : int (** private_blocks - public_blocks *)
      ; diff_votes : int (** private_votes - public_votes *)
      ; diff_depth : int (** private_depth - public_depth *)
      ; include_foreign_votes : bool
      }
    [@@deriving fields]

    let length = List.length Fields.names

    let low =
      { public_blocks = 0
      ; public_votes = 0
      ; public_depth = 0
      ; private_blocks = 0
      ; private_votes = 0
      ; private_depth = 0
      ; diff_blocks = min_int
      ; diff_votes = min_int
      ; diff_depth = min_int
      ; include_foreign_votes = false
      }
    ;;

    let high =
      { public_blocks = max_int
      ; public_votes = max_int
      ; public_depth = max_int
      ; private_blocks = max_int
      ; private_votes = max_int
      ; private_depth = max_int
      ; diff_blocks = max_int
      ; diff_votes = min_int
      ; diff_depth = max_int
      ; include_foreign_votes = true
      }
    ;;

    let to_floatarray t =
      let a = Float.Array.make length Float.nan in
      let set conv i field =
        Float.Array.set a i (Fieldslib.Field.get field t |> conv);
        i + 1
      in
      let int = set float_of_int
      and bool = set (fun x -> if x then 1. else 0.) in
      let _ =
        Fields.fold
          ~init:0
          ~public_blocks:int
          ~public_votes:int
          ~public_depth:int
          ~private_blocks:int
          ~private_votes:int
          ~private_depth:int
          ~diff_blocks:int
          ~diff_votes:int
          ~diff_depth:int
          ~include_foreign_votes:bool
      in
      a
    ;;

    let of_floatarray =
      let get conv _ i = (fun a -> Float.Array.get a i |> conv), i + 1 in
      let int = get int_of_float
      and bool =
        get (fun f ->
            match int_of_float f with
            | 0 -> false
            | _ -> true)
      in
      fst
        (Fields.make_creator
           0
           ~public_blocks:int
           ~public_votes:int
           ~public_depth:int
           ~private_blocks:int
           ~private_votes:int
           ~private_depth:int
           ~diff_blocks:int
           ~diff_votes:int
           ~diff_depth:int
           ~include_foreign_votes:bool)
    ;;

    let to_string t =
      let conv to_s field =
        Printf.sprintf
          "%s: %s"
          (Fieldslib.Field.name field)
          (to_s (Fieldslib.Field.get field t))
      in
      let int = conv string_of_int
      and bool = conv string_of_bool in
      Fields.to_list
        ~public_blocks:int
        ~public_votes:int
        ~public_depth:int
        ~private_blocks:int
        ~private_votes:int
        ~private_depth:int
        ~diff_blocks:int
        ~diff_votes:int
        ~diff_depth:int
        ~include_foreign_votes:bool
      |> String.concat "\n"
    ;;

    let%test _ =
      let run _i =
        let t =
          { public_blocks = Random.bits ()
          ; public_votes = Random.bits ()
          ; public_depth = Random.bits ()
          ; private_blocks = Random.bits ()
          ; private_votes = Random.bits ()
          ; private_depth = Random.bits ()
          ; diff_blocks = Random.bits ()
          ; diff_votes = Random.bits ()
          ; diff_depth = Random.bits ()
          ; include_foreign_votes = Random.bool ()
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

    type state =
      { public : env Dag.vertex (* defender's preferred block *)
      ; private_ : env Dag.vertex (* attacker's preferred block *)
      ; epoch : [ `Proceed | `Prolong ]
            (* Proceed: the attacker considers the defender's votes that extend on his
               preferred block when building a new block.

               Prolong: the attacker prolongs the current epoch until he can form a block
               that does not reference any defender votes. *)
      ; pending_private_to_public_messages : env Dag.vertex list
      }

    type observable_state =
      { state : state
      ; common : env Dag.vertex
      }

    let preferred (s : state) = s.private_

    let init ~roots =
      let module N = Honest (V) in
      let genesis = N.init ~roots in
      { public = genesis
      ; private_ = genesis
      ; epoch = `Prolong
      ; pending_private_to_public_messages = []
      }
    ;;

    (* the attacker emulates a defending node. This is the local_view of the defender *)

    module Public_view = struct
      include V

      let rec visibility x =
        released x
        || (not (appended_by_me x))
        || (is_summary x && List.for_all visibility (Dag.parents view x))
      ;;

      let view = Dag.filter visibility view

      let appended_by_me _vertex =
        (* The attacker simulates an honest node on the public view. This node should not
           interpret attacker vertices as own vertices. *)
        false
      ;;
    end

    module Public = Honest (Public_view)

    (* the attacker emulates a defending node. This describes the defender node *)
    let handle_public (s : state) event =
      let public = (Public.handler s.public event).state in
      { s with public }
    ;;

    module Private = Honest (V)

    let vote_filter state x =
      match state.epoch with
      | `Proceed -> true
      | `Prolong -> appended_by_me x
    ;;

    let puzzle_payload (s : state) =
      let vote_filter = vote_filter s in
      Private.puzzle_payload' ~vote_filter s.private_
    ;;

    let prepare (state : state) event =
      let state =
        (* deliver to defender the messages sent by attacker during last step *)
        List.fold_left
          (fun state msg -> handle_public state (Deliver msg))
          { state with pending_private_to_public_messages = [] }
          state.pending_private_to_public_messages
      and attempt_summary state =
        let vote_filter = vote_filter state in
        match Private.next_summary' ~vote_filter state.private_ with
        | Some private_ -> { state with private_ }
        | None -> state
      in
      match event with
      | PuzzleSolved _ ->
        (* work on private chain *)
        attempt_summary state
      | Deliver _x ->
        let state =
          (* simulate defender *)
          handle_public state event
        in
        attempt_summary state
    ;;

    let prepare s e =
      { state = prepare s e
      ; common = Dag.common_ancestor view s.public s.private_ |> Option.get
      }
    ;;

    let observe { state = s; common } =
      let open Observation in
      let private_depth, private_votes =
        Dag.iterate_descendants votes_only [ s.private_ ]
        |> Seq.fold_left (fun (d, n) x -> max d (depth x), n + 1) (0, 0)
      and public_depth, public_votes =
        Dag.iterate_descendants votes_only [ s.public ]
        |> Seq.fold_left (fun (d, n) x -> max d (depth x), n + 1) (0, 0)
      in
      let ca_height = height common
      and private_height = height s.private_
      and public_height = height s.public in
      { private_blocks = private_height - ca_height
      ; public_blocks = public_height - ca_height
      ; diff_blocks = private_height - public_height
      ; private_votes
      ; public_votes
      ; diff_votes = private_votes - public_votes
      ; private_depth
      ; public_depth
      ; diff_depth = private_depth - public_depth
      ; include_foreign_votes =
          (match s.epoch with
          | `Proceed -> true
          | `Prolong -> false)
      }
    ;;

    let interpret { state = s; common } action =
      let release kind =
        let module Map = Map.Make (Int) in
        let rec h release_now seq =
          match seq () with
          | Seq.Nil -> release_now (* override/match not possible; release all *)
          | Seq.Cons (x, seq) ->
            let release_now' = Map.add (Dag.id x) x release_now in
            let module N =
              Honest (struct
                include V

                let visibility x = released x || Map.mem (Dag.id x) release_now'
                let view = Dag.filter visibility view
              end)
            in
            if s.public
               $!= (N.handler
                      s.public
                      (Deliver x) (* this is stateful. Deliver might add summary! *))
                     .state
            then (
              (* release_now' is just enough to override; release_now was not enough; *)
              match kind with
              | `Override -> release_now'
              | `Match -> release_now)
            else h release_now' seq
        in
        Dag.iterate_descendants view [ common ]
        |> Seq.filter (fun x -> not (released x))
        |> h Map.empty
        |> Map.bindings
        |> List.map snd
      in
      match (action : Action.t) with
      | Adopt_Proceed -> [], { s with epoch = `Proceed; private_ = s.public }
      | Adopt_Prolong -> [], { s with epoch = `Prolong; private_ = s.public }
      | Match_Proceed -> release `Match, { s with epoch = `Proceed }
      | Match_Prolong -> release `Match, { s with epoch = `Prolong }
      | Override_Proceed -> release `Override, { s with epoch = `Proceed }
      | Override_Prolong -> release `Override, { s with epoch = `Prolong }
      | Wait_Proceed -> [], { s with epoch = `Proceed }
      | Wait_Prolong -> [], { s with epoch = `Prolong }
    ;;

    let conclude (pending_private_to_public_messages, state) =
      { share = pending_private_to_public_messages
      ; state = { state with pending_private_to_public_messages }
      }
    ;;

    let apply state action = interpret state action |> conclude
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
      else if o.private_depth = 0 && o.private_blocks = o.public_blocks + 1
      then Override_Proceed
      else if o.public_blocks = o.private_blocks && o.private_votes = o.public_votes + 1
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
