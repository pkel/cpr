open Cpr_lib.Next

type data = { height : int }

let key = "nakamoto"
let info = "Nakamoto consensus"
let puzzles_per_block = 1
let height data = data.height
let describe { height } = Printf.sprintf "block %i" height
let dag_roots = [ { height = 0 } ]

let dag_validity (type a) ((module V) : (a, data) global_view) vertex =
  let open V in
  match pow_hash vertex, Dag.parents view vertex with
  | Some _, [ p ] ->
    let child = data vertex
    and p = data p in
    child.height = p.height + 1
  | _ -> false
;;

let judge (type a) ((module V) : (a, data) global_view) l =
  let open V in
  (* TODO pick longest chain *)
  List.to_seq l |> Cpr_lib.Dag.common_ancestor' view |> Option.get
;;

module Honest (V : LocalView with type data = data) = struct
  include V

  type state = env Dag.vertex

  let preferred state = state

  let init ~roots =
    match roots with
    | [ genesis ] -> genesis
    | _ -> failwith "invalid roots"
  ;;

  let puzzle_payload state =
    { sign = false; parents = [ state ]; data = { height = (data state).height + 1 } }
  ;;

  let handler state = function
    | PuzzleSolved vertex -> { state = vertex; share = [ vertex ] }
    | Deliver vertex ->
      let consider = data vertex
      and preferred = data state in
      { share = []
      ; state = (if consider.height > preferred.height then vertex else state)
      }
  ;;
end

let honest (type a) ((module V) : (a, data) local_view) : (a, data) node =
  Node (module Honest (V))
;;

let constant c : ('env, data) reward_function = fun ~view:_ ~assign n -> assign c n

let reward_functions () =
  let open Collection in
  empty |> add ~info:"1 per confirmed block" "block" (constant 1.)
;;

module SszAttack = struct
  let info = "SSZ'16 attack space"

  module Observation = struct
    type t =
      { public_blocks : int (** number of blocks after common ancestor *)
      ; private_blocks : int (** number of blocks after common ancestor *)
      ; diff_blocks : int (** private_blocks - public_blocks *)
      }
    [@@deriving fields]

    let length = List.length Fields.names
    let low = { public_blocks = 0; private_blocks = 0; diff_blocks = min_int }

    let high =
      { public_blocks = max_int; private_blocks = max_int; diff_blocks = max_int }
    ;;

    let to_floatarray t =
      let a = Float.Array.make length Float.nan in
      let set conv i field =
        Float.Array.set a i (Fieldslib.Field.get field t |> conv);
        i + 1
      in
      let int = set float_of_int in
      let _ =
        Fields.fold ~init:0 ~public_blocks:int ~private_blocks:int ~diff_blocks:int
      in
      a
    ;;

    let of_floatarray =
      let get conv _ i = (fun a -> Float.Array.get a i |> conv), i + 1 in
      let int = get int_of_float in
      fst (Fields.make_creator 0 ~public_blocks:int ~private_blocks:int ~diff_blocks:int)
    ;;

    let to_string t =
      let conv to_s field =
        Printf.sprintf
          "%s: %s"
          (Fieldslib.Field.name field)
          (to_s (Fieldslib.Field.get field t))
      in
      let int = conv string_of_int in
      Fields.to_list ~public_blocks:int ~private_blocks:int ~diff_blocks:int
      |> String.concat "\n"
    ;;

    let%test _ =
      let run _i =
        let t =
          { public_blocks = Random.bits ()
          ; private_blocks = Random.bits ()
          ; diff_blocks = Random.bits ()
          }
        in
        t = (to_floatarray t |> of_floatarray)
      in
      List.init 50 run |> List.for_all (fun x -> x)
    ;;
  end

  module Action = struct
    type t =
      | Adopt
          (** Adopt the defender's preferred chain as attacker's preferred chain. Withheld
              attacker blocks are discarded.

              Equivalent to SSZ'16 model. *)
      | Override
          (** Publish just enough information to make the defender adopt the chain just
              released. The attacker continues mining the private chain.

              If override is impossible, this still results in a release of withheld
              information.

              Equivalent to SSZ'16 model. *)
      | Match
          (** Publish just enough information such that the defender observes a tie
              between two chains. The attacker continues mining the private chain.

              If match is impossible, this still results in a release of withheld
              information.

              Equivalent to SSZ'16 model. *)
      | Wait (** Continue withholding. Always possible. Equivalent to SSZ'16 model. *)
    [@@deriving variants]

    let to_string = Variants.to_name
    let to_int = Variants.to_rank

    let table =
      let add acc var = var.Variantslib.Variant.constructor :: acc in
      Variants.fold ~init:[] ~adopt:add ~override:add ~match_:add ~wait:add
      |> List.rev
      |> Array.of_list
    ;;

    let of_int i = table.(i)
    let n = Array.length table
  end

  module Agent (V : LocalView with type data = data) = struct
    include V

    module State : sig
      type t = private
        { public : env Dag.vertex (* defender's preferred block *)
        ; private_ : env Dag.vertex (* attacker's preferred block *)
        ; common : env Dag.vertex (* common chain *)
        }

      val init : env Dag.vertex -> t

      (* Set fields in state; updates common chain *)
      val update : ?public:env Dag.vertex -> ?private_:env Dag.vertex -> t -> t
    end = struct
      type t =
        { public : env Dag.vertex
        ; private_ : env Dag.vertex
        ; common : env Dag.vertex
        }

      let init x = { public = x; private_ = x; common = x }

      (* call this whenever public or private_ changes *)
      let set_common state =
        let common = Dag.common_ancestor view state.public state.private_ in
        assert (Option.is_some common) (* all our protocols maintain this invariant *);
        { state with common = Option.get common }
      ;;

      let update ?public ?private_ t =
        set_common
          { public = Option.value ~default:t.public public
          ; private_ = Option.value ~default:t.private_ private_
          ; common = t.common
          }
      ;;
    end

    type state = State.t
    type observable_state = Observable of state

    let preferred (s : state) = s.private_

    let puzzle_payload (s : state) =
      let module N = Honest (V) in
      N.puzzle_payload s.private_
    ;;

    let init ~roots =
      let module N = Honest (V) in
      N.init ~roots |> State.init
    ;;

    (* the attacker emulates a defending node. This is the local_view of the defender *)

    let public_visibility (s : state) x =
      Dag.partial_order s.common x >= 0 || not (appended_by_me x)
    ;;

    let public_view (s : state) : (env, data) local_view =
      (module struct
        include V

        let view = Dag.filter (public_visibility s) view

        (* The attacker simulates an honest node on the public view. This node should not
           interpret attacker vertices as own vertices. *)
        let appended_by_me _vertex = false
      end)
    ;;

    (* the attacker works on a subset of the total information: he ignores new defender
       blocks *)

    let private_visibility (s : state) x =
      Dag.partial_order s.common x >= 0 || appended_by_me x
    ;;

    let private_view (s : state) : (env, data) local_view =
      (module struct
        include V

        let view = Dag.filter (private_visibility s) view
      end)
    ;;

    (* the attacker emulates a defending node. This describes the defender node *)
    let handle_public (s : state) event =
      let (module V) = public_view s in
      let open Honest (V) in
      let public = (handler s.public event).state in
      State.update ~public s
    ;;

    (* the attacker emulates a another node. This describes the defender node *)
    let handle_private (s : state) event =
      let (module V) = private_view s in
      let open Honest (V) in
      let private_ = (handler s.private_ event).state in
      State.update ~private_ s
    ;;

    let prepare state event =
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
        if private_visibility state x then handle_private state event else state
    ;;

    let prepare s e = Observable (prepare s e)

    let observe (Observable s) =
      let open Observation in
      let block_height vertex = height (data vertex) in
      let ca_height = block_height s.common
      and private_height = block_height s.private_
      and public_height = block_height s.public in
      { private_blocks = private_height - ca_height
      ; public_blocks = public_height - ca_height
      ; diff_blocks = private_height - public_height
      }
    ;;

    let interpret (s : state) action =
      let parent vtx =
        match Dag.parents view vtx with
        | [ x ] -> Some x
        | _ -> None
      in
      let match_ offset =
        let h =
          (* height of to be released block *)
          height (data s.public) + offset
        in
        (* look for to be released block backwards from private head *)
        let rec f b = if height (data b) <= h then b else parent b |> Option.get |> f in
        [ f s.private_ ]
        (* NOTE: if private height is smaller target height, then private head is
           released. *)
      in
      match (action : Action.t) with
      | Adopt -> [], State.update ~private_:s.public s
      | Match -> match_ 0, s
      | Override -> match_ 1, s
      | Wait -> [], s
    ;;

    let conclude (share, state) =
      let simulate_public state msg = handle_public state (Deliver msg) in
      { share; state = List.fold_left simulate_public state share }
    ;;

    let apply (Observable state) action = interpret state action |> conclude
  end

  let agent (type a) policy ((module V) : (a, data) local_view) : (a, data) node =
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
      if o.private_blocks > o.public_blocks
      then Override
      else if o.private_blocks < o.public_blocks
      then Adopt
      else Wait
    ;;

    (* Patrik's ad-hoc strategy *)
    let simple o =
      let open Observation in
      let open Action in
      if o.public_blocks > 0
      then if o.private_blocks < o.public_blocks then Adopt else Override
      else Wait
    ;;

    (* Eyal and Sirer. Majority is not enough: Bitcoin mining is vulnerable. 2014. *)
    let es_2014 o =
      (* I interpret this from the textual description of the strategy. There is an
         algorithmic version in the paper, but it depends on the observation whether the
         last mined block is honest or not. *)
      let open Observation in
      let open Action in
      if o.private_blocks < o.public_blocks
      then (* 1. *) Adopt
      else if o.public_blocks = 0 && o.private_blocks = 1
      then (* 2. *) Wait
      else if o.public_blocks = 1 && o.private_blocks = 1
      then (* 3. *) Match
      else if o.public_blocks = 1 && o.private_blocks = 2
      then (* 4. *) Override
      else if o.public_blocks = 2 && o.private_blocks = 1
      then (* 5. Redundant: included in 1. *)
        Adopt
      else (
        (* The attacker established a lead of more than two before: *)
        let _ = () in
        if o.public_blocks > 0
        then
          if o.private_blocks - o.public_blocks = 1
          then (* 6. *) Override
          else (* 7. *) Match
        else Wait)
    ;;

    (* Sapirshtein, Sompolinsky, Zohar. Optimal Selfish Mining Strategies in Bitcoin.
       2016. *)
    let ssz_2016_sm1 o =
      (* The authors rephrase the policy of ES'14 and call it SM1. Their version is much
         shorter.

         The authors define an MDP to find better strategies for various parameters alpha
         and gamma. We cannot reproduce this here in this module. Our RL framework should
         be able to find these policies, though. *)
      let open Observation in
      let open Action in
      match o.public_blocks, o.private_blocks with
      | h, a when h > a -> Adopt
      | 1, 1 -> Match
      | h, a when h = a - 1 && h >= 1 -> Override
      | _ (* Otherwise *) -> Wait
    ;;
  end

  let policies =
    let open Collection in
    let open Policies in
    empty
    |> add ~info:"emulate honest behaviour" "honest" honest
    |> add ~info:"simple withholding policy" "simple" simple
    |> add ~info:"Eyal and Sirer 2014" "eyal-sirer-2014" es_2014
    |> add ~info:"Sapirshtein et al. 2016, SM1" "sapirshtein-2016-sm1" ssz_2016_sm1
  ;;
end

let attacks () =
  Collection.map
    (fun { key; info; it } ->
      { key = "ssz-" ^ key; info = SszAttack.info ^ "; " ^ info; it = SszAttack.agent it })
    SszAttack.policies
;;
