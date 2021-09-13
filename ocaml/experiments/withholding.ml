open Cpr_lib
open Models

let protocols =
  let k = [ 1; 2; 4; 8; 16; 32; 64; 128 ] in
  nakamoto :: List.concat_map (fun k -> [ bk ~k; bk_lessleader ~k; george ~k ]) k
;;

let alphas = [ 0.1; 0.2; 0.25; 0.33; 0.4; 0.45; 0.5 ]
let block_intervals = [ 600. ]

let params ~n_activations ~block_interval ~protocol =
  Simulator.
    { activations = n_activations
    ; activation_delay = block_interval /. (protocol.pow_per_block |> float_of_int)
    }
;;

(* Run all combinations of protocol, attack, network and block_interval. *)
let tasks ~n_activations =
  List.concat_map
    (fun (P protocol) ->
      List.concat_map
        (fun net ->
          List.concat_map
            (fun block_interval ->
              let params = params ~n_activations ~block_interval ~protocol in
              let open Csv_runner in
              Collection.map_to_list
                (fun attack ->
                  Task
                    { params
                    ; protocol
                    ; attack = Some attack
                    ; sim = net protocol attack params
                    })
                protocol.attacks)
            block_intervals)
        (List.map (fun alpha -> two_agents ~alpha) alphas))
    protocols
;;

let () = Csv_runner.command tasks
