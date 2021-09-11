open Cpr_lib
open Models

let protocols =
  let k = [ 1; 2; 4; 8; 16; 32; 64; 128 ] in
  nakamoto :: List.concat_map (fun k -> [ bk ~k; bk_lessleader ~k; george ~k ]) k
;;

let networks = [ CliqueUniform10 ]
let block_intervals = [ 30.; 60.; 120.; 300.; 600. ]

let models ~n_activations ~protocol =
  List.concat_map
    (fun network ->
      List.map
        (fun block_interval ->
          { network
          ; activations = n_activations
          ; scenario = AllHonest
          ; activation_delay = block_interval /. (protocol.pow_per_block |> float_of_int)
          })
        block_intervals)
    networks
;;

(* Run all combinations of protocol, network and block_interval. *)
let tasks ~n_activations =
  List.concat_map
    (fun (P protocol) ->
      List.concat_map
        (fun model ->
          Collection.map_to_list
            (fun attack -> Csv_runner.Task { model; protocol; attack = Some attack })
            protocol.attacks)
        (models ~n_activations ~protocol))
    protocols
;;

let () = Csv_runner.command tasks
