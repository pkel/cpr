open Cpr_lib.Next

let%test "nakamoto_progress" =
  let open Simulator in
  let propagation_delay = Distributions.exponential ~ev:1. in
  let test (activation_delay, height) =
    let network = Network.T.symmetric_clique ~activation_delay ~propagation_delay 32 in
    let env = init (module Nakamoto) network in
    loop ~activations:1000 env;
    Array.to_seq env.nodes
    |> Seq.map (fun (Node x) -> x.preferred x.state)
    |> Dag.common_ancestor' env.global_view
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
