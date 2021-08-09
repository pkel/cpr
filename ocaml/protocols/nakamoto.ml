open Cpr_lib

type block = { height : int }

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

let protocol : _ protocol =
  let preferred x = x in
  let honest v = { handler = handler v; init; preferred } in
  { dag_roots; dag_validity; honest }
;;

let%test "convergence" =
  let open Simulator in
  let test params height =
    let env = all_honest params protocol |> init in
    loop params env;
    Array.to_seq env.nodes
    |> Seq.map (fun (Node x) -> x.preferred x.state)
    |> Dag.common_ancestor' env.global.view
    |> function
    | None -> false
    | Some n -> (Dag.data n).value.height > height
  in
  let delay = Distributions.exponential ~ev:1. in
  List.for_all
    (fun (activation_delay, height) ->
      let network = Network.homogeneous ~delay 32 in
      test { network; activations = 10000; activation_delay } height)
    [ 10., 9000 (* good condition, 10% orphans *)
    ; 01., 5000
      (* bad conditions, 50% orphans *)
    ]
;;

let constant c : ('env, block) reward_function = fun ~view:_ ~assign n -> assign c n
