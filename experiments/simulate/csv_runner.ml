open Cpr_lib

type task =
  | Task :
      { activations : int
      ; network : Network.t
      ; protocol : (module Protocol with type data = 'dag_data)
      ; attack :
          (('dag_data Simulator.block, 'dag_data) view
           -> ('dag_data Simulator.block, 'dag_data) node)
          Collection.entry
          option
      ; sim : (unit -> 'dag_data Simulator.state) Collection.entry
      }
      -> task

let save_rows_as_tsv filename l =
  Bos.OS.File.with_oc
    (Fpath.v filename)
    (fun oc () ->
      Ok
        (Format.fprintf
           (Format.formatter_of_out_channel oc)
           "%a"
           (Info.pp_rows ~sep:"\t")
           l))
    ()
  |> Rresult.R.join
  |> Rresult.R.failwith_error_msg
;;

let array k f arr = Info.string k (Array.to_list arr |> List.map f |> String.concat "|")

let floatarray k arr =
  Info.string k (Float.Array.to_list arr |> List.map string_of_float |> String.concat "|")
;;

let prepare_row (Task { activations; network; protocol; attack; sim }) =
  let (module Protocol) = protocol in
  let strategy, strategy_description =
    match attack with
    | Some x -> x.Collection.key, x.info
    | None -> "none", ""
  and compute = Array.map (fun x -> x.Network.compute) network.nodes in
  let open Info in
  [ string "network" sim.key
  ; string "network_description" sim.info
  ; float "activation_delay" network.activation_delay
  ; array "compute" string_of_float compute
  ; int "number_activations" activations
  ; string "strategy" strategy
  ; string "strategy_description" strategy_description
  ; string "version" Cpr_lib.version
  ]
  @ Info.map_key
      (function
        | "family" -> "protocol"
        | x -> x)
      Protocol.info
;;

let run task =
  let row = prepare_row task in
  let (Task t) = task in
  let (module Protocol) = t.protocol in
  let clock = Mtime_clock.counter () in
  try
    let open Simulator in
    let sim = t.sim.it () in
    let (module Ref) = sim.referee in
    (* simulate *)
    loop ~activations:t.activations sim;
    let head = head sim in
    let headd = Dag.data head in
    row
    @ Info.
        [ float "machine_duration_s" (Mtime_clock.count clock |> Mtime.Span.to_s)
        ; array "activations" string_of_int sim.activations
        ; floatarray "reward" headd.rewards
        ; float "head_time" (timestamp headd)
        ; float "head_progress" (Ref.progress head)
        ]
    @ Info.prefix_key "head_" (Ref.info head)
  with
  | e ->
    let bt = Printexc.get_backtrace () in
    let () =
      ignore (failwith "TODO. Print protocol.info");
      Printf.eprintf
        "\nRUN:\tnetwork:%s\tprotocol:%s\tstrategy:%s\n"
        t.sim.key
        Protocol.key
        (match t.attack with
        | None -> "n/a"
        | Some a -> a.key);
      Printf.eprintf "ERROR:\t%s\n%s" (Printexc.to_string e) bt;
      flush stderr
    in
    row
    @ Info.
        [ string "error" (Printexc.to_string e)
        ; float "machine_duration_s" (Mtime_clock.count clock |> Mtime.Span.to_s)
        ]
;;

let main tasks n_activations n_cores filename =
  let tasks = tasks ~n_activations in
  let n_tasks = List.length tasks in
  let queue = ref tasks in
  let acc = ref [] in
  Printf.eprintf "Run %d simulations in parallel\n" n_cores;
  Progress.with_reporter (Common.progress_bar n_tasks) (fun progress ->
      if n_cores > 1
      then
        Parany.run
          n_cores
          ~demux:(Common.parany_demux_list_ref queue)
          ~work:(fun task -> run task)
          ~mux:(fun l ->
            progress 1;
            acc := l :: !acc)
      else
        acc
          := List.rev_map
               (fun task ->
                 let l = run task in
                 progress 1;
                 l)
               tasks);
  let rows = List.rev !acc in
  save_rows_as_tsv filename rows
;;

open Cmdliner

let filename =
  let doc = "Name of CSV output file (tab-separated)." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"OUTPUT" ~doc)
;;

let main_t tasks =
  Term.(const (main tasks) $ Common.activations $ Common.cores $ filename)
;;
