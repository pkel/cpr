open Cpr_lib
module Protocol = Ethereum
open Protocol

let key = "ssz"
let info = "SSZ'16-like attack space"

module Observation = struct
  type t =
    { public_height : int
          (** defender chain, number of sequential blocks after common ancestor *)
    ; public_progress : int
          (** defender chain, number of blocks after common ancestor, including uncles *)
    ; public_orphans : int
          (** number of orphans that could be included as uncles in the public chain *)
    ; private_height : int
          (** attacker chain, number of sequential blocks after common ancestor *)
    ; private_progress : int
          (** attacker chain, number of blocks after common ancestor, including uncles *)
    ; private_orphans : int
          (** number of orphans that could be included as uncles in the attacker chain *)
    ; diff_height : int (** private_height - public_height *)
    ; diff_progress : int (** private_progress - public_progress *)
    ; diff_orphans : int (** private_orphans - public_orphans *)
    }
  [@@deriving fields]

  let length = List.length Fields.names

  let low =
    { public_height = 0
    ; public_progress = 0
    ; public_orphans = 0
    ; private_height = 0
    ; private_progress = 0
    ; private_orphans = 0
    ; diff_height = min_int
    ; diff_progress = min_int
    ; diff_orphans = min_int
    }
  ;;

  let high =
    { public_height = max_int
    ; public_progress = max_int
    ; public_orphans = max_int
    ; private_height = max_int
    ; private_progress = max_int
    ; private_orphans = max_int
    ; diff_height = max_int
    ; diff_progress = max_int
    ; diff_orphans = max_int
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
        ~public_height:int
        ~public_progress:int
        ~public_orphans:int
        ~private_height:int
        ~private_progress:int
        ~private_orphans:int
        ~diff_height:int
        ~diff_progress:int
        ~diff_orphans:int
    in
    a
  ;;

  let of_floatarray =
    let get conv _ i = (fun a -> Float.Array.get a i |> conv), i + 1 in
    let int = get int_of_float in
    fst
      (Fields.make_creator
         0
         ~public_height:int
         ~public_progress:int
         ~public_orphans:int
         ~private_height:int
         ~private_progress:int
         ~private_orphans:int
         ~diff_height:int
         ~diff_progress:int
         ~diff_orphans:int)
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
      ~public_height:int
      ~public_progress:int
      ~public_orphans:int
      ~private_height:int
      ~private_progress:int
      ~private_orphans:int
      ~diff_height:int
      ~diff_progress:int
      ~diff_orphans:int
    |> String.concat "\n"
  ;;

  let%test _ =
    let run _i =
      let t =
        { public_height = Random.bits ()
        ; public_progress = Random.bits ()
        ; public_orphans = Random.bits ()
        ; private_height = Random.bits ()
        ; private_progress = Random.bits ()
        ; private_orphans = Random.bits ()
        ; diff_height = Random.bits ()
        ; diff_progress = Random.bits ()
        ; diff_orphans = Random.bits ()
        }
      in
      t = (to_floatarray t |> of_floatarray)
    in
    List.init 50 run |> List.for_all (fun x -> x)
  ;;
end

module Action = struct
  (* Bitcoin has Adopt, Override, Match, and Wait Actions. Ethereum adds a couple of
     choices:

     1. attacker can release own blocks on adopt (or not) with the intention that the
     defender adopts it as uncle.

     2. attacker can include foreign orphans or ignore them

     . *)
  type action =
    | Adopt_discard
        (** Adopt the defender's preferred chain as attacker's preferred chain. Withheld
            attacker blocks are discarded.

            Equivalent to Adopt in SSZ'16 model. *)
    | Adopt_release
        (** Adopt the defender's preferred chain as attacker's preferred chain. Withheld
            attacker blocks are released.

            No equivalent in SSZ'16 model. *)
    | Override
        (** Publish just enough information to make the defender adopt the chain just
            released. The attacker continues mining the private chain.

            If override is impossible, this still results in a release of withheld
            information.

            Equivalent to Override in SSZ'16 model. *)
    | Match
        (** Publish just enough information such that the defender observes a tie between
            two chains. The attacker continues mining the private chain.

            If match is impossible, this still results in a release of withheld
            information.

            Equivalent to Match in SSZ'16 model. *)
    | Wait
        (** Continue withholding. Always possible. Equivalent to Wait in SSZ'16 model. *)
  [@@deriving variants]

  let action_to_string = Variants_of_action.to_name
  let action_to_int = Variants_of_action.to_rank

  let action_list =
    let add acc var = var.Variantslib.Variant.constructor :: acc in
    Variants_of_action.fold
      ~init:[]
      ~adopt_discard:add
      ~adopt_release:add
      ~override:add
      ~match_:add
      ~wait:add
    |> List.rev
  ;;

  type uncles =
    { own : bool
    ; foreign : bool
    }
  [@@deriving fields]

  let uncles_to_string { own; foreign } =
    let string_of_bool b = if b then "true" else "false" in
    Printf.sprintf
      "uncles {own: %s; foreign: %s}"
      (string_of_bool own)
      (string_of_bool foreign)
  ;;

  let uncles_list =
    let bools = [ false; true ] in
    List.concat_map (fun own -> List.map (fun foreign -> { own; foreign }) bools) bools
  ;;

  type t = action * uncles

  let to_string (action, uncles) =
    action_to_string action ^ ", " ^ uncles_to_string uncles
  ;;

  let table =
    List.concat_map
      (fun action -> List.map (fun uncles -> action, uncles) uncles_list)
      action_list
    |> Array.of_list
  ;;

  let of_int i = table.(i)
  let n = Array.length table

  let to_int =
    let ht = Hashtbl.create n in
    let () = Array.iteri (fun i x -> Hashtbl.add ht x i) table in
    Hashtbl.find ht
  ;;

  let%test_unit "to_int" =
    let hit = Array.make n false in
    let () = Array.iter (fun a -> hit.(to_int a) <- true) table in
    assert (Array.for_all (fun x -> x) hit)
  ;;

  let%test_unit "of_int" =
    for i = 0 to n - 1 do
      let a = of_int i in
      assert (i == to_int a)
    done
  ;;
end

module Agent (V : LocalView with type data = data) = struct
  open Protocol.Referee (V)
  include V

  module State : sig
    type t = private
      { public : env Dag.vertex (* defender's preferred block *)
      ; private_ : env Dag.vertex (* attacker's preferred block *)
      ; common : env Dag.vertex (* common chain *)
      ; pending_private_to_public_messages : env Dag.vertex list
      ; mining : Action.uncles
      }

    val init : mining:Action.uncles -> env Dag.vertex -> t

    (* Set fields in state; updates common chain *)
    val update
      :  ?public:env Dag.vertex
      -> ?private_:env Dag.vertex
      -> ?pending_private_to_public_messages:env Dag.vertex list
      -> ?mining:Action.uncles
      -> t
      -> t
  end = struct
    type t =
      { public : env Dag.vertex
      ; private_ : env Dag.vertex
      ; common : env Dag.vertex
      ; pending_private_to_public_messages : env Dag.vertex list
      ; mining : Action.uncles
      }

    let init ~mining x =
      { public = x
      ; private_ = x
      ; common = x
      ; pending_private_to_public_messages = []
      ; mining
      }
    ;;

    (* call this whenever public or private_ changes *)
    let set_common state =
      let common = Dag.common_ancestor view state.public state.private_ in
      assert (Option.is_some common) (* all our protocols maintain this invariant *);
      { state with common = Option.get common }
    ;;

    let update ?public ?private_ ?pending_private_to_public_messages ?mining t =
      set_common
        { public = Option.value ~default:t.public public
        ; private_ = Option.value ~default:t.private_ private_
        ; common = t.common
        ; pending_private_to_public_messages =
            Option.value
              ~default:t.pending_private_to_public_messages
              pending_private_to_public_messages
        ; mining = Option.value ~default:t.mining mining
        }
    ;;
  end

  type state = State.t
  type observable_state = Observable of state

  let preferred (s : state) = s.private_

  let init ~roots =
    let module N = Honest (V) in
    N.init ~roots |> State.init ~mining:{ own = true; foreign = true }
  ;;

  (* the attacker emulates a defending node. This is the local_view of the defender *)

  module Public_view = struct
    include V

    let visibility x = released x
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
    State.update ~public s
  ;;

  module Private = Honest (V)

  let puzzle_payload (s : state) =
    (* reuse honest logic for locating orphans, then filter them according to agent choice *)
    let honest = Private.puzzle_payload s.private_ in
    let parents =
      List.filteri
        (fun i x ->
          i == 0
          || (s.mining.own && appended_by_me x)
          || (s.mining.foreign && not (appended_by_me x)))
        honest.parents
    in
    let n_uncles = List.length parents - 1
    and p = List.hd parents |> data in
    { honest with
      parents
    ; data = { honest.data with progress = p.progress + n_uncles + 1 }
    }
  ;;

  let handle_private (s : state) event =
    let private_ = (Private.handler s.private_ event).state in
    State.update ~private_ s
  ;;

  let prepare (state : state) event =
    let state =
      let pending = state.pending_private_to_public_messages in
      List.fold_left
        (fun state msg -> handle_public state (Deliver msg))
        (State.update ~pending_private_to_public_messages:[] state)
        pending
    in
    let state =
      match event with
      | PuzzleSolved _ ->
        (* work on private chain *)
        handle_private state event
      | Deliver x ->
        let state =
          (* simulate defender *)
          handle_public state event
        in
        (* deliver visible (not ignored) messages *)
        if Public_view.visibility x then handle_private state event else state
    in
    Observable state
  ;;

  let observe (Observable s) =
    let open Observation in
    let private_proposal = Private.puzzle_payload s.private_
    and public_proposal = Public.puzzle_payload s.public in
    let common = data s.common
    and private_ = data s.private_
    and public = data s.public in
    let public_height = public.height - common.height
    and public_progress = public.progress - common.progress
    and public_orphans = List.length public_proposal.parents - 1
    and private_height = private_.height - common.height
    and private_progress = private_.progress - common.progress
    and private_orphans = List.length private_proposal.parents - 1 in
    { public_height
    ; public_progress
    ; public_orphans
    ; private_height
    ; private_progress
    ; private_orphans
    ; diff_height = private_height - public_height
    ; diff_progress = private_progress - public_progress
    ; diff_orphans = private_orphans - public_orphans
    }
  ;;

  let interpret (s : state) (action, mining) =
    let parent vtx =
      match Dag.parents view vtx with
      | hd :: _tl -> Some hd
      | _ -> None
    in
    let match_ offset =
      let progress =
        (* progress of to be released block *)
        (data s.public).progress + offset
      in
      (* look for to be released block backwards from private head *)
      let rec f b =
        if (data b).progress <= progress then b else parent b |> Option.get |> f
      in
      [ f s.private_ ]
      (* NOTE: if private progress is smaller target height, then private head is
         released. *)
    in
    let release, state =
      match (action : Action.action) with
      | Adopt_release -> [ s.private_ ], State.update ~private_:s.public s
      | Adopt_discard -> [], State.update ~private_:s.public s
      | Match -> match_ 0, s
      | Override -> match_ 1, s
      | Wait -> [], s
    in
    release, State.update ~mining state
  ;;

  let conclude (pending_private_to_public_messages, state) =
    { share = pending_private_to_public_messages
    ; state = State.update ~pending_private_to_public_messages state
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
  let honest o =
    let open Observation in
    let open Action in
    let mining = { own = true; foreign = true } in
    if o.public_progress > 0 then Adopt_release, mining else Override, mining
  ;;

  let selfish ?(adopt = `Release) o =
    (* Ad-hoc strategy. This is probably not optimal. *)
    let open Observation in
    let open Action in
    let mining = { own = true; foreign = false } in
    let adopt =
      match adopt with
      | `Release -> Adopt_release
      | `Discard -> Adopt_discard
    in
    if o.private_progress < o.public_progress
    then adopt, mining
    else if o.private_progress = 0 && o.public_progress = 0
    then Wait, mining
    else if o.public_progress = 0
    then Wait, mining
    else Override, mining
  ;;
end

let policies =
  let open Collection in
  let open Policies in
  empty
  |> add ~info:"emulate honest behaviour" "honest" honest
  |> add
       ~info:"ad-hoc selfish policy w/ release on adopt"
       "selfish_release"
       (selfish ~adopt:`Release)
  |> add
       ~info:"ad-hoc selfish policy w/ discard on adopt"
       "selfish_discard"
       (selfish ~adopt:`Discard)
;;
