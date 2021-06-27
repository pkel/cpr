open Cpr_lib
open Protocol

type block = { height : int }

let dag_roots = [ { height = 0 } ]

let dag_invariant ~pow parents child =
  match pow, parents with
  | true, [ p ] -> child.height = p.height + 1
  | _ -> false
;;

let init ~roots =
  match roots with
  | [ genesis ] -> genesis
  | _ -> failwith "invalid roots"
;;

let handler ctx preferred =
  let data n = Dag.data n |> ctx.read in
  function
  | Activate pow ->
    let head = data preferred in
    let head' = ctx.extend_dag ~pow [ preferred ] { height = head.height + 1 } in
    ctx.share head';
    head'
  | Deliver gnode ->
    (* Only consider gnode if its heritage is visible. *)
    if Dag.have_common_ancestor ctx.view gnode preferred
    then (
      let consider preferred gnode =
        let node = data gnode
        and head = data preferred in
        if node.height > head.height then gnode else preferred
      in
      (* delayed gnode might connect nodes delivered previously *)
      List.fold_left consider preferred (Dag.leaves ctx.view gnode))
    else preferred
;;

let protocol : _ protocol =
  let spawn ctx = { handler = handler ctx; init } in
  { dag_roots; dag_invariant; spawn }
;;

let%test "convergence" =
  let open Simulator in
  let test params height =
    init params protocol
    |> loop params
    |> fun { nodes; global_view; _ } ->
    Array.to_seq nodes
    |> Seq.map (fun x -> x.state)
    |> Dag.common_ancestor' global_view
    |> function
    | None -> false
    | Some n -> (Dag.data n).value.height > height
  in
  List.for_all
    (fun (activation_delay, height) ->
      let network = Network.homogeneous ~delay:(Distributions.exponential ~ev:1.) 32 in
      test { network; n_activations = 10000; activation_delay } height)
    [ 10., 9000 (* good condition, 10% orphans *)
    ; 01., 5000 (* bad conditions, 50% orphans *)
    ]
;;
