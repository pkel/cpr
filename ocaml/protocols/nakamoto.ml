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

module Ssz16compat = struct
  open Ssz16compat

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
      let () = assert (ca $== s.common) (* TODO. Eliminate call of common_ancestor *) in
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

  let honest_policy o =
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

  (* Sapirshtein, Sompolinsky, Zohar. Optimal Selfish Mining Strategies in Bitcoin. 2016. *)
  let ssz_2016_sm1 o =
    (* The authors rephrase the policy of ES'14 and call it SM1. Their version is much
       shorter.

       The authors define an MDP to find better strategies for various parameters alpha
       and gamma. We cannot reproduce this here in this module. Our RL framework should be
       able to find these policies, though. *)
    let open Observation in
    let open Action in
    match o.public_blocks, o.private_blocks with
    | h, a when h > a -> Adopt
    | 1, 1 -> Match
    | h, a when h = a - 1 && h >= 1 -> Override
    | _ (* Otherwise *) -> Wait
  ;;

  (* TODO: check GKWGRC'16 for better strategies *)

  let policies =
    [ "honest", honest_policy
    ; "simple", simple
    ; "eyal-sirer-2014", es_2014
    ; "sapirshtein-2016-sm1", ssz_2016_sm1
    ]
  ;;

  (* This strategy was designed for the PrivateAttack module. It does not work for
     Ssz16compat!*)
  let _selfish_policy' v state =
    let open Action in
    if state.private_ $== state.public
    then Wait
    else (
      let ca = Dag.common_ancestor v.view state.private_ state.public |> Option.get in
      if ca $== state.public then Wait else Override)
  ;;

  let apply_action v state action =
    let parent v n =
      match Dag.parents v.view n with
      | [ x ] -> Some x
      | _ -> None
    in
    let match_ offset =
      let height =
        (* height of to be released block *)
        let v = public_view state v in
        block_height v state.public + offset
      in
      (* look for to be released block backwards from private head *)
      let rec h b =
        if block_height v b <= height then b else parent v b |> Option.get |> h
      in
      Some (h state.private_)
      (* NOTE: if private height is smaller target height, then private head is released. *)
    in
    match (action : Action.t) with
    | Adopt -> { release = None; adopt = true }
    | Match -> { release = match_ 0; adopt = false }
    | Override -> { release = match_ 1; adopt = false }
    | Wait -> { release = None; adopt = false }
  ;;

  let lift_policy p (v : _ local_view) state : Action.t = Observation.observe v state |> p
  let tactic_of_policy p v state = (lift_policy p) v state |> apply_action v state
  let tactic_of_policy' p v state = p v state |> apply_action v state

  let attack p = Node (attack ~honest (tactic_of_policy p))
  and attack' p = Node (attack ~honest (tactic_of_policy' p))
end

let attacks =
  let open Collection in
  empty
  |> add
       ~info:"SSZ'16 compatible attack model with honest policy"
       "private-honest"
       Ssz16compat.(attack honest_policy)
  |> add
       ~info:"SSZ'16 compatible attack model with simple selfish mining policy"
       "private-simple"
       Ssz16compat.(attack simple)
  |> add
       ~info:"SSZ'16 compatible attack model with ES'14 selfish mining policy"
       "private-eyal-sirer-2014"
       Ssz16compat.(attack es_2014)
  |> add
       ~info:"SSZ'16 compatible attack model with SSZ'16 SM1 policy"
       "private-sapirshtein-2016-sm1"
       Ssz16compat.(attack ssz_2016_sm1)
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
  let propagation_delay = Distributions.exponential ~ev:1. in
  let test (activation_delay, height) =
    let network = Network.homogeneous ~activation_delay ~propagation_delay 32 in
    let env = all_honest network protocol |> init in
    loop ~activations:1000 env;
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
