open Cpr_lib
open Models

let protocols =
  let k = [ 1; 2; 4; 8; 16; 32; 64; 128 ] in
  nakamoto :: List.concat_map (fun k -> [ bk ~k; bk_lessleader ~k; george ~k ]) k
;;

let alphas = [ 0.1; 0.2; 0.25; 0.33; 0.4; 0.45; 0.5 ]

(* Run all combinations of protocol, attack, network and block_interval. *)
let tasks ~n_activations =
  List.concat_map
    (fun (P protocol) ->
      List.concat_map
        (fun net ->
          Collection.map_to_list
            (fun attack ->
              let sim, network = net protocol attack in
              Csv_runner.Task
                { activations = n_activations
                ; protocol
                ; attack = Some attack
                ; sim
                ; network
                })
            protocol.attacks)
        (List.map (fun alpha -> two_agents ~alpha) alphas))
    protocols
;;

open Cmdliner
open Common

let info =
  let doc = "simulate withholding strategies against proof-of-work protocols" in
  Term.info ~version ~doc "withholding"
;;

let () = Term.exit @@ Term.eval (Csv_runner.main_t tasks, info)
