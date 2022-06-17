open Cpr_lib.Next
open Models

let alphas = [ 0.1; 0.2; 0.25; 0.33; 0.4; 0.45; 0.5 ]

let two_agents (type a) (module A : AttackSpace with type Protocol.data = a) n_activations
  =
  let protocol = (module A.Protocol : Protocol with type data = a) in
  List.concat_map
    (fun net ->
      Collection.map_to_list
        (fun policy ->
          let attack =
            Collection.
              { key = A.key ^ "-" ^ policy.key
              ; info = A.info ^ "; " ^ policy.info
              ; it = A.attacker policy.it
              }
          in
          let sim, network = net protocol attack in
          Csv_runner.Task
            { activations = n_activations; protocol; attack = Some attack; sim; network })
        A.policies)
    (List.map (fun alpha -> two_agents ~alpha) alphas)
;;

let selfish_mining
    (type a)
    (module A : AttackSpace with type Protocol.data = a)
    n_activations
  =
  let protocol = (module A.Protocol : Protocol with type data = a) in
  let gammas = [ 0.; 0.25; 0.5; 0.75; 0.9 ] in
  List.concat_map
    (fun net ->
      Collection.map_to_list
        (fun policy ->
          let attack =
            Collection.
              { key = A.key ^ "-" ^ policy.key
              ; info = A.info ^ "; " ^ policy.info
              ; it = A.attacker policy.it
              }
          in
          let sim, network = net protocol attack in
          Csv_runner.Task
            { activations = n_activations; protocol; attack = Some attack; sim; network })
        A.policies)
    (List.concat_map
       (fun alpha ->
         List.map (fun gamma -> selfish_mining ~defenders:10 ~alpha gamma) gammas)
       alphas)
;;

(* Run all combinations of protocol, attack, network and block_interval. *)
let tasks ~n_activations =
  let open Cpr_protocols in
  let k = [ 1; 2; 4; 8; 16; 32; 64 ] in
  let nakamoto =
    two_agents (module Nakamoto.SSZattack) n_activations
    @ selfish_mining (module Nakamoto.SSZattack) n_activations
  and bk =
    List.concat_map
      (fun k ->
        let module P =
          Bk.Make (struct
            let k = k
          end)
        in
        two_agents (module P.SSZattack) n_activations)
      k
  in
  List.concat [ nakamoto; bk ]
;;

open Cmdliner

let info =
  let doc = "simulate withholding strategies against proof-of-work protocols" in
  Term.info ~version ~doc "withholding"
;;

let () = Term.exit @@ Term.eval (Csv_runner.main_t tasks, info)
