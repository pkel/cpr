open Cpr_lib.Next
open Models

let block_intervals = [ 30.; 60.; 120.; 300.; 600. ]

let activation_delay (type a) ~block_interval ~protocol =
  let (module Protocol : Protocol with type data = a) = protocol in
  block_interval /. (Protocol.puzzles_per_block |> float_of_int)
;;

let tasks_per_protocol protocol n_activations =
  List.concat_map
    (fun net ->
      List.map
        (fun block_interval ->
          let activation_delay = activation_delay ~block_interval ~protocol in
          let sim, network = net ~activation_delay protocol in
          Csv_runner.Task
            { activations = n_activations; protocol; attack = None; sim; network })
        block_intervals)
    [ honest_clique ~n:10 ]
;;

(* Run all combinations of protocol, network and block_interval. *)
let tasks ~n_activations =
  let open Cpr_protocols in
  let bk =
    List.concat_map
      (fun k ->
        let module P =
          Bk.Make (struct
            let k = k
          end)
        in
        tasks_per_protocol (module P) n_activations)
      [ 1; 2; 4; 8; 16; 32; 64; 128 ]
  and nakamoto = tasks_per_protocol (module Nakamoto) n_activations in
  List.concat [ nakamoto; bk ]
;;

open Cmdliner

let info =
  let doc = "simulate honest networks running proof-of-work protocols" in
  Term.info ~version ~doc "honest_net"
;;

let () = Term.exit @@ Term.eval (Csv_runner.main_t tasks, info)
