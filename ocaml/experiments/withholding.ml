open Cpr_lib
open Models

let alphas = [ 0.1; 0.2; 0.25; 0.33; 0.4; 0.45; 0.5 ]

let two_agents (AttackSpace (module A)) n_activations =
  let protocol = (module A.Protocol : Protocol with type data = _) in
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

let selfish_mining (AttackSpace (module A)) n_activations =
  let protocol = (module A.Protocol : Protocol with type data = _) in
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
  let k = [ 1; 2; 4; 8; 16; 32 ] in
  let nakamoto =
    two_agents nakamoto_ssz n_activations @ selfish_mining nakamoto_ssz n_activations
  and ethereum =
    two_agents ethereum_ssz n_activations @ selfish_mining ethereum_ssz n_activations
  and bk = List.concat_map (fun k -> two_agents (bk_ssz ~k) n_activations) k
  and bkll = List.concat_map (fun k -> two_agents (bkll_ssz ~k) n_activations) k
  and tailstorm =
    List.concat_map
      (fun rewards ->
        List.concat_map (fun k -> two_agents (tailstorm_ssz ~rewards ~k) n_activations) k
        @ List.concat_map
            (fun k -> selfish_mining (tailstorm_ssz ~rewards ~k) n_activations)
            k)
      Tailstorm.reward_schemes
  and tailstorm' =
    List.concat_map
      (fun rewards ->
        List.concat_map
          (fun k -> two_agents (tailstorm_draft ~rewards ~k) n_activations)
          k)
      Tailstorm.reward_schemes
  in
  List.concat [ nakamoto; ethereum; bk; bkll; tailstorm; tailstorm' ]
;;

open Cmdliner

let info =
  let doc = "simulate withholding strategies against proof-of-work protocols" in
  Cmd.info ~version ~doc "withholding"
;;

let () = Csv_runner.main_t tasks |> Cmd.v info |> Cmd.eval |> Stdlib.exit
