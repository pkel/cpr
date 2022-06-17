open Cpr_lib.Next

let%test_unit "bk_progress" =
  let open Simulator in
  let propagation_delay = Distributions.exponential ~ev:1. in
  let test (k, activation_delay, height) =
    let module P =
      Bk.Make (struct
        let k = k
      end)
    in
    let n_nodes = 32 in
    let network =
      Network.T.symmetric_clique ~activation_delay ~propagation_delay n_nodes
    in
    let env = init (module P) network in
    loop ~activations:(1000 * P.puzzles_per_block) env;
    let head = head env in
    let is = P.height (Dag.data head).value in
    if is < height then failwith (Printf.sprintf "not enough progress: %i/%i" is height)
  in
  List.iter
    test
    [ 08, 10., 900 (* good condition, 10% orphans *)
    ; 08, 01., 700 (* bad conditions, 30% orphans *)
    ; 32, 01., 900
      (* bad conditions, 10% orphans, high k *)
    ]
;;
