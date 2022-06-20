open Cpr_lib.Next
open Models

let block_intervals = [ 30.; 60.; 120.; 300.; 600. ]

let activation_delay ~block_interval ~puzzles_per_block =
  block_interval /. (puzzles_per_block |> float_of_int)
;;

let tasks_per_protocol ~n_activations (Protocol protocol) =
  let (module P) = protocol in
  List.concat_map
    (fun net ->
      List.map
        (fun block_interval ->
          let activation_delay =
            let open P in
            activation_delay ~block_interval ~puzzles_per_block
          in
          let sim, network = net ~activation_delay protocol in
          Csv_runner.Task
            { activations = n_activations; protocol; attack = None; sim; network })
        block_intervals)
    [ honest_clique ~n:10 ]
;;

let protocols =
  let open Cpr_protocols in
  nakamoto
  :: List.concat_map (fun k -> [ bk ~k; bkll ~k ]) [ 1; 2; 4; 8; 16; 32; 64; 128 ]
;;

(* Run all combinations of protocol, network and block_interval. *)
let tasks ~n_activations = List.concat_map (tasks_per_protocol ~n_activations) protocols

open Cmdliner

let info =
  let doc = "simulate honest networks running proof-of-work protocols" in
  Term.info ~version ~doc "honest_net"
;;

let () = Term.exit @@ Term.eval (Csv_runner.main_t tasks, info)
