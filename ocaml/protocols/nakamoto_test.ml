open Cpr_lib.Next

let%test_unit "nakamoto_progress" =
  let open Simulator in
  let propagation_delay = Distributions.exponential ~ev:1. in
  let test (activation_delay, height) =
    let n_nodes = 32 in
    let network =
      Network.T.symmetric_clique ~activation_delay ~propagation_delay n_nodes
    in
    let env = init (module Nakamoto) network in
    loop ~activations:1000 env;
    let head = head env in
    let is = (Dag.data head).value.height in
    if is < height then failwith (Printf.sprintf "not enough progress: %i/%i" is height)
  in
  List.iter
    test
    [ 16., 900 (* good condition, 10% orphans *)
    ; 01., 500 (* bad conditions, 50% orphans *)
    ]
;;
