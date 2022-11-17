open Cpr_lib
open Models

let alphas = [ 0.1; 0.2; 0.25; 0.33; 0.4; 0.45; 0.5 ]

let two_agents n_activations (AttackSpace (module A)) =
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

let selfish_mining n_activations (AttackSpace (module A)) =
  let protocol = (module A.Protocol : Protocol with type data = _) in
  (* let gammas = [ 0.; 0.25; 0.5; 0.75; 0.9 ] in *)
  let gammas = [ 0.; 0.5; 0.75; 0.9 ] in
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
         List.map
           (fun gamma ->
             let defenders = 1. /. (1. -. gamma) |> Float.ceil |> Float.to_int in
             selfish_mining ~defenders ~alpha gamma)
           gammas)
       alphas)
;;

(* Run all combinations of protocol, attack, network and block_interval. *)
let tasks ~n_activations =
  let two_agents = two_agents n_activations
  and selfish_mining = selfish_mining n_activations in
  let open Cpr_protocols in
  two_agents nakamoto_ssz
  @ selfish_mining nakamoto_ssz
  @ two_agents (ethereum_ssz ~incentive_scheme:`Discount)
  @ selfish_mining (ethereum_ssz ~incentive_scheme:`Discount)
  @ List.concat_map
      (fun k ->
        List.concat_map
          (fun incentive_scheme ->
            two_agents (bk_ssz ~k ~incentive_scheme)
            @ two_agents (bkll_ssz ~k ~incentive_scheme))
          [ `Block; `Constant ]
        @ List.concat_map
            (fun incentive_scheme ->
              let subblock_selection = if k > 8 then `Heuristic else `Optimal in
              two_agents (tailstorm_ssz ~subblock_selection ~incentive_scheme ~k)
              @ two_agents (tailstormll_ssz ~subblock_selection ~incentive_scheme ~k))
            [ `Constant; `Discount ])
      [ 1; 2; 4; 8; 16; 32 ]
;;

open Cmdliner

let info =
  let doc = "simulate withholding strategies against proof-of-work protocols" in
  Cmd.info ~version ~doc "withholding"
;;

let () = Csv_runner.main_t tasks |> Cmd.v info |> Cmd.eval |> Stdlib.exit
