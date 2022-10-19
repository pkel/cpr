open Cpr_lib
open Models

let activation_delays = [ 30.; 60.; 120.; 300.; 600. ]

let tasks_per_protocol ~n_activations (Protocol protocol) =
  let (module P) = protocol in
  List.concat_map
    (fun net ->
      List.map
        (fun activation_delay ->
          let sim, network = net ~activation_delay protocol in
          Csv_runner.Task
            { activations = n_activations; protocol; attack = None; sim; network })
        activation_delays)
    [ honest_clique ~n:10 ]
;;

let protocols =
  let open Cpr_protocols in
  nakamoto
  :: ethereum ~incentive_scheme:`Discount
  :: List.concat_map
       (fun k ->
         List.concat_map
           (fun incentive_scheme ->
             [ bk ~k ~incentive_scheme; bkll ~k ~incentive_scheme ])
           [ `Block; `Constant ]
         @ List.concat_map
             (fun incentive_scheme ->
               let subblock_selection = if k > 8 then `Heuristic else `Optimal in
               [ tailstorm ~subblock_selection ~incentive_scheme ~k
               ; tailstormll ~subblock_selection ~incentive_scheme ~k
               ])
             [ `Constant; `Discount ])
       [ 1; 2; 4; 8; 16; 32 ]
;;

(* Run all combinations of protocol, network and block_interval. *)
let tasks ~n_activations = List.concat_map (tasks_per_protocol ~n_activations) protocols

open Cmdliner

let info =
  let doc = "simulate honest networks running proof-of-work protocols" in
  Cmd.info ~version ~doc "honest_net"
;;

let () = Csv_runner.main_t tasks |> Cmd.v info |> Cmd.eval |> Stdlib.exit
