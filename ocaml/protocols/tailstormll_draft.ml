open Cpr_lib

module Make (Parameters : Tailstormll.Parameters) = struct
  module Protocol = Tailstormll.Make (Parameters)
  open Protocol

  let key = "draft"
  let info = "draft withholding attack space"

  module Observation = struct
    type t =
      { public_blocks : int (** number of blocks after common ancestor *)
      ; public_depth : int (** number of votes confirming the leading block *)
      ; private_blocks : int (** number of blocks after common ancestor *)
      ; private_depth : int (** number of votes confirming the leading block *)
      ; diff_blocks : int (** private_blocks - public_blocks *)
      ; diff_depth : int (** private_votes - public_votes *)
      }
    [@@deriving fields]

    let length = List.length Fields.names

    let low =
      { public_blocks = 0
      ; public_depth = 0
      ; private_blocks = 0
      ; private_depth = 0
      ; diff_blocks = min_int
      ; diff_depth = min_int
      }
    ;;

    let high =
      { public_blocks = max_int
      ; public_depth = max_int
      ; private_blocks = max_int
      ; private_depth = max_int
      ; diff_blocks = max_int
      ; diff_depth = max_int
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
          ~public_depth:int
          ~private_blocks:int
          ~private_depth:int
          ~diff_blocks:int
          ~diff_depth:int
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
           ~public_depth:int
           ~private_blocks:int
           ~private_depth:int
           ~diff_blocks:int
           ~diff_depth:int)
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
        ~public_depth:int
        ~private_blocks:int
        ~private_depth:int
        ~diff_blocks:int
        ~diff_depth:int
      |> String.concat "\n"
    ;;

    let%test _ =
      let run _i =
        let t =
          { public_blocks = Random.bits ()
          ; public_depth = Random.bits ()
          ; private_blocks = Random.bits ()
          ; private_depth = Random.bits ()
          ; diff_blocks = Random.bits ()
          ; diff_depth = Random.bits ()
          }
        in
        t = (to_floatarray t |> of_floatarray)
      in
      List.init 50 run |> List.for_all (fun x -> x)
    ;;
  end

  module Action = struct
    type t =
      | Release
          (** Release up to preferred private block and all withheld votes for this block.
              Used to model honest strategy. *)
      | Override
          (** Publish just enough information to make the defender adopt the chain just
              released. The attacker continues mining the private chain.

              If override is impossible, this still results in a release of withheld
              information. *)
      | Match
          (** Publish just enough information such that the defender observes a tie
              between two chains. The attacker continues mining the private chain.

              If override is impossible, this still results in a release of withheld
              information. *)
      | Wait (** Continue withholding. Always possible. *)
    [@@deriving variants]

    let to_string = Variants.to_name
    let to_int = Variants.to_rank

    let table =
      let add acc var = var.Variantslib.Variant.constructor :: acc in
      Variants.fold ~init:[] ~override:add ~match_:add ~wait:add ~release:add
      |> List.rev
      |> Array.of_list
    ;;

    let of_int i = table.(i)
    let n = Array.length table
  end

  module Agent (V : LocalView with type data = data) = struct
    open Protocol.Referee (V)
    include V

    type state =
      { public : env Dag.vertex (* private/withheld tip of chain *)
      ; private_ : env Dag.vertex (* public/defender tip of chain *)
      ; pending_private_to_public_messages : env Dag.vertex list
      }

    type observable_state = Observable of state

    let preferred state = last_block state.private_

    let init ~roots =
      let module N = Honest (V) in
      let i = N.init ~roots in
      { public = i; private_ = i; pending_private_to_public_messages = [] }
    ;;

    (* the attacker emulates a defending node. This is the local_view of the defender *)

    let public_visibility x = delivered x || released x

    module Public = Honest (struct
      include V

      let view = Dag.filter public_visibility view
      let appended_by_me _vertex = false

      (* The attacker simulates an honest node on the public view. This node should not
         interpret attacker vertices as own vertices. *)
    end)

    (* the attacker emulates a defending node. This describes the defender node *)
    let handle_public (s : state) event =
      let public = (Public.handler s.public event).state in
      { s with public }
    ;;

    (* the attacker acts honestly on all available information *)

    module Private = Honest (V)

    let handle_private (s : state) event =
      let private_ = (Private.handler s.private_ event).state in
      { s with private_ }
    ;;

    let puzzle_payload (s : state) = Private.puzzle_payload s.private_

    let prepare (state : state) event =
      let state =
        List.fold_left
          (fun state msg -> handle_public state (Deliver msg))
          { state with pending_private_to_public_messages = [] }
          state.pending_private_to_public_messages
      in
      match event with
      | PuzzleSolved _ ->
        (* work on private chain *)
        handle_private state event
      | Deliver _ ->
        let state =
          (* simulate defender *)
          handle_public state event
        in
        (* deliver visible (not ignored) messages *)
        handle_private state event
    ;;

    let prepare s e = Observable (prepare s e)

    let observe (Observable s) =
      let open Observation in
      let private_depth = (data s.private_).vote
      and public_depth = (data s.public).vote in
      let ca = Dag.common_ancestor view s.private_ s.public |> Option.get |> last_block in
      let ca_height = height ca
      and private_height = height s.private_
      and public_height = height s.public in
      { private_blocks = private_height - ca_height
      ; public_blocks = public_height - ca_height
      ; diff_blocks = private_height - public_height
      ; private_depth
      ; public_depth
      ; diff_depth = private_depth - public_depth
      }
    ;;

    let interpret state action =
      let parent n =
        match Dag.parents view n with
        | hd :: _ -> Some hd
        | _ -> None
      in
      let match_ ~and_override () =
        let cmp =
          Compare.(
            by (tuple int int) (fun x ->
                let x = data x in
                x.block, x.vote))
        in
        let x =
          (* find node to be released backwards from private head *)
          let rec h x x' =
            let d = cmp x state.public in
            if d <= 0
            then if and_override then x' else x
            else h (parent x |> Option.get) x
          in
          h state.private_ state.private_
          (* NOTE: if private height is smaller public height, then private head is marked
             for release. *)
        in
        [ x ]
      in
      match (action : Action.t) with
      | Wait -> [], `PreferPrivate, state
      | Match -> match_ ~and_override:false (), `PreferPrivate, state
      | Override -> match_ ~and_override:true (), `PreferPrivate, state
      | Release -> Dag.leaves view (last_block state.private_), `PreferPrivate, state
    ;;

    let conclude (pending_private_to_public_messages, x, state) =
      let state =
        match x with
        | `PreferPrivate -> state
        | `PreferPublic -> { state with private_ = state.public }
      in
      { share = pending_private_to_public_messages
      ; state = { state with pending_private_to_public_messages }
      }
    ;;

    let apply (Observable state) action = interpret state action |> conclude
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
    let honest_policy _o = Action.Release

    let release_block_policy o =
      let open Observation in
      let open Action in
      if o.private_blocks > o.public_blocks then Override else Wait
    ;;

    let override_block_policy o =
      let open Observation in
      let open Action in
      if o.private_blocks = 0 && o.public_blocks = 0
      then Wait
      else if o.public_blocks = 0
      then Wait
      else Override
    ;;

    let override_catchup_policy o =
      let open Observation in
      let open Action in
      if o.private_blocks = 0 && o.public_blocks = 0
      then Wait
      else if o.public_blocks = 0
      then Wait
      else if o.private_depth = 0 && o.private_blocks = o.public_blocks + 1
      then Override
      else if o.public_blocks = o.private_blocks && o.private_depth = o.public_depth + 1
      then Override
      else if o.private_blocks - o.public_blocks > 10
              (* fork can become really deep for strong attackers. Cut-off shortens time
                 spent in common ancestor computation. *)
      then Override
      else Wait
    ;;
  end

  let policies =
    let open Policies in
    let open Collection in
    empty
    |> add ~info:"emulate honest behaviour" "honest" honest_policy
    |> add ~info:"release private block a.s.a.p." "release-block" release_block_policy
    |> add ~info:"override public block a.s.a.p." "override-block" override_block_policy
    |> add
         ~info:"override public head just before defender catches up"
         "override-catchup"
         override_catchup_policy
  ;;
end
