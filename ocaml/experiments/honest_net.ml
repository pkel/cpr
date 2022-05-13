open Cpr_lib
open Models

let protocols =
  let k = [ 1; 2; 4; 8; 16; 32; 64; 128 ] in
  nakamoto :: List.concat_map (fun k -> [ bk ~k; bk_lessleader ~k; george ~k ]) k
;;

let block_intervals = [ 30.; 60.; 120.; 300.; 600. ]

let activation_delay ~block_interval ~protocol =
  block_interval /. (protocol.pow_per_block |> float_of_int)
;;

(* Run all combinations of protocol, network and block_interval. *)
let tasks ~n_activations =
  List.concat_map
    (fun (P protocol) ->
      List.concat_map
        (fun net ->
          List.map
            (fun block_interval ->
              let activation_delay = activation_delay ~block_interval ~protocol in
              let sim, network = net ~activation_delay protocol in
              Csv_runner.Task
                { activations = n_activations; protocol; attack = None; sim; network })
            block_intervals)
        [ honest_clique ~n:10 ])
    protocols
;;

open Cmdliner

let info =
  let doc = "simulate honest networks running proof-of-work protocols" in
  Term.info ~version ~doc "honest_net"
;;

let () = Term.exit @@ Term.eval (Csv_runner.main_t tasks, info)
