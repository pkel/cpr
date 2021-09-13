open Cpr_lib
open Models

let protocols =
  let k = [ 1; 2; 4; 8; 16; 32; 64; 128 ] in
  nakamoto :: List.concat_map (fun k -> [ bk ~k; bk_lessleader ~k; george ~k ]) k
;;

let block_intervals = [ 30.; 60.; 120.; 300.; 600. ]

let params ~n_activations ~block_interval ~protocol =
  Simulator.
    { activations = n_activations
    ; activation_delay = block_interval /. (protocol.pow_per_block |> float_of_int)
    }
;;

(* Run all combinations of protocol, network and block_interval. *)
let tasks ~n_activations =
  List.concat_map
    (fun (P protocol) ->
      List.concat_map
        (fun net ->
          List.map
            (fun block_interval ->
              let params = params ~n_activations ~block_interval ~protocol in
              Csv_runner.Task
                { params; protocol; attack = None; sim = net protocol params })
            block_intervals)
        [ honest_clique ~n:10 ])
    protocols
;;

let () = Csv_runner.command tasks
