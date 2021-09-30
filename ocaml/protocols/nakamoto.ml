open Cpr_lib

type dag_data = { height : int }

let height x = x.height
let describe { height } = Printf.sprintf "block %i" height
let dag_roots = [ { height = 0 } ]

let dag_validity (v : _ global_view) n =
  match v.pow_hash n, Dag.parents v.view n with
  | Some _, [ p ] ->
    let child = v.data n
    and p = v.data p in
    child.height = p.height + 1
  | _ -> false
;;

let init ~roots =
  match roots with
  | [ genesis ] -> genesis
  | _ -> failwith "invalid roots"
;;

let block_height v vtx = (v.data vtx).height

let handler v actions preferred = function
  | Activate pow ->
    let head = v.data preferred in
    let head' = actions.extend_dag ~pow [ preferred ] { height = head.height + 1 } in
    actions.share head';
    head'
  | Deliver gnode ->
    let node = v.data gnode
    and head = v.data preferred in
    if node.height > head.height then gnode else preferred
;;

let honest v =
  let preferred x = x in
  { handler = handler v; init; preferred }
;;

let constant c : ('env, dag_data) reward_function = fun ~view:_ ~assign n -> assign c n

let reward_functions =
  let open Collection in
  empty |> add ~info:"1 per confirmed block" "block" (constant 1.)
;;

module PrivateAttack = struct
  open PrivateAttack

  module Observation = struct
    type t =
      { public_blocks : int (** number of blocks after common ancestor *)
      ; private_blocks : int (** number of blocks after common ancestor *)
      ; diff_blocks : int (** private_blocks - public_blocks *)
      }
    [@@deriving fields]

    let length = List.length Fields.names

    let observe v s =
      let ca = Dag.common_ancestor v.view s.private_ s.public |> Option.get in
      let ca_height = block_height v ca
      and private_height = block_height v s.private_
      and public_height = block_height v s.public in
      { private_blocks = private_height - ca_height
      ; public_blocks = public_height - ca_height
      ; diff_blocks = private_height - public_height
      }
    ;;

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

  let honest_policy _o = Action.Release

  let selfish_policy o =
    let open Observation in
    let open Action in
    if o.private_blocks = 0 && o.public_blocks = 0
    then Wait
    else if o.public_blocks = 0
    then Wait
    else Override
  ;;

  let policies = [ "honest", honest_policy; "selfish", selfish_policy ]

  let selfish_policy' v state =
    let open Action in
    if state.private_ $== state.public
    then Wait
    else (
      let ca = Dag.common_ancestor v.view state.private_ state.public |> Option.get in
      if ca $== state.public then Wait else Override)
  ;;

  let apply_action v ~release state =
    let parent v n =
      match Dag.parents v.view n with
      | [ x ] -> Some x
      | _ -> None
    in
    let match_ ~and_override () =
      let height =
        let v = public_view v in
        block_height v state.public + if and_override then 1 else 0
      in
      let block =
        (* find block to be released backwards from private head *)
        let rec h b =
          if block_height v b <= height then b else parent v b |> Option.get |> h
        in
        h state.private_
        (* NOTE: if private height is smaller public height, then private head is marked
           for release. *)
      in
      release_recursive v release [ block ]
    in
    fun a ->
      match (a : Action.t) with
      | Wait -> `PreferPrivate
      | Match ->
        match_ ~and_override:false ();
        `PreferPrivate
      | Override ->
        match_ ~and_override:true ();
        `PreferPrivate
      | Release ->
        release_recursive v release [ state.private_ ];
        `PreferPrivate
  ;;

  let lift_policy p (v : _ local_view) state : Action.t = Observation.observe v state |> p

  let tactic_of_policy p v ~release state =
    (lift_policy p) v state |> apply_action v ~release state
  ;;

  let tactic_of_policy' p v ~release state = p v state |> apply_action v ~release state

  let attack p = Node (attack honest (tactic_of_policy p))
  and attack' p = Node (attack honest (tactic_of_policy' p))
end

let attacks =
  let open Collection in
  empty
  |> add
       ~info:"Private attack with honest policy"
       "private-honest"
       PrivateAttack.(attack honest_policy)
  |> add
       ~info:"Private attack with selfish policy"
       "private-selfish"
       PrivateAttack.(attack selfish_policy)
  |> add
       ~info:"Private attack with selfish policy (alternative policy implementation)"
       "private-selfish-alt"
       PrivateAttack.(attack' selfish_policy')
;;

let protocol : _ protocol =
  { key = "nakamoto"
  ; info = "Nakamoto consensus"
  ; pow_per_block = 1
  ; honest
  ; dag_validity
  ; dag_roots
  ; describe
  ; height
  ; reward_functions
  ; attacks
  }
;;

let%test "convergence" =
  let open Simulator in
  let delay = Distributions.exponential ~ev:1. in
  let network = Network.homogeneous ~delay 32 in
  let test (activation_delay, height) =
    let params = { activations = 1000; activation_delay } in
    let env = all_honest params network protocol |> init in
    loop params env;
    Array.to_seq env.nodes
    |> Seq.map (fun (Node x) -> x.preferred x.state)
    |> Dag.common_ancestor' env.global.view
    |> function
    | None -> false
    | Some n -> (Dag.data n).value.height > height
  in
  List.for_all
    test
    [ 10., 900 (* good condition, 10% orphans *)
    ; 01., 500
      (* bad conditions, 50% orphans *)
    ]
;;
