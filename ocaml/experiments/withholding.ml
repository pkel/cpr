open Cpr_lib
open Models

let protocols =
  let k = [ 1; 2; 4; 8; 16; 32; 64; 128 ] in
  nakamoto :: List.concat_map (fun k -> [ bk ~k; bk_lessleader ~k; george ~k ]) k
;;

let networks =
  [ 0.1; 0.2; 0.25; 0.33; 0.4; 0.45; 0.5 ]
  |> List.map (fun alpha -> TwoAgentsZero { alpha })
;;

let block_intervals = [ 600. ]

let models ~n_activations ~protocol =
  List.concat_map
    (fun network ->
      List.map
        (fun block_interval ->
          { network
          ; activations = n_activations
          ; scenario = FirstSelfish
          ; activation_delay = block_interval /. (protocol.pow_per_block |> float_of_int)
          })
        block_intervals)
    networks
;;

(* Run all combinations of protocol, attack, network and block_interval. *)
let tasks ~n_activations =
  List.concat_map
    (fun (P protocol) ->
      List.concat_map
        (fun model ->
          let open Csv_runner in
          Task { model; protocol; attack = None }
          :: Collection.map_to_list
               (fun attack -> Task { model; protocol; attack = Some attack })
               protocol.attacks)
        (models ~n_activations ~protocol))
    protocols
;;

let () = Csv_runner.command tasks
