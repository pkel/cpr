open Cpr_lib.Next
open Models

let protocols =
  (*
  let k = [ 1; 2; 4; 8; 16; 32; 64 ] in
  nakamoto :: List.concat_map (fun k -> [ bk ~k; bk_lessleader ~k; tailstorm ~k ]) k
  *)
  ()
  [@@ocamlformat "wrap-comments=false"]
;;

let alphas = [ 0.1; 0.2; 0.25; 0.33; 0.4; 0.45; 0.5 ]

let two_agents (type a) protocol n_activations =
  let (module Protocol : Protocol with type data = a) = protocol in
  List.concat_map
    (fun net ->
      Collection.map_to_list
        (fun attack ->
          let sim, network = net protocol attack in
          Csv_runner.Task
            { activations = n_activations; protocol; attack = Some attack; sim; network })
        (Protocol.attacks ()))
    (List.map (fun alpha -> two_agents ~alpha) alphas)
;;

let selfish_mining (type a) protocol n_activations =
  let (module Protocol : Protocol with type data = a) = protocol in
  let gammas = [ 0.; 0.25; 0.5; 0.75; 0.9 ] in
  List.concat_map
    (fun net ->
      Collection.map_to_list
        (fun attack ->
          let sim, network = net protocol attack in
          Csv_runner.Task
            { activations = n_activations; protocol; attack = Some attack; sim; network })
        (Protocol.attacks ()))
    (List.concat_map
       (fun alpha ->
         List.map (fun gamma -> selfish_mining ~defenders:10 ~alpha gamma) gammas)
       alphas)
;;

(* Run all combinations of protocol, attack, network and block_interval. *)
let tasks ~n_activations =
  let open Cpr_protocols in
  List.concat
    (* TODO add the other protocols here *)
    [ two_agents (module Nakamoto) n_activations
    ; selfish_mining (module Nakamoto) n_activations
    ]
;;

open Cmdliner

let info =
  let doc = "simulate withholding strategies against proof-of-work protocols" in
  Term.info ~version ~doc "withholding"
;;

let () = Term.exit @@ Term.eval (Csv_runner.main_t tasks, info)
