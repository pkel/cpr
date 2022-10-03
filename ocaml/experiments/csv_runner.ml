open Cpr_lib

type task =
  | Task :
      { activations : int
      ; network : Network.t
      ; protocol : (module Protocol with type data = 'dag_data)
      ; attack :
          (('dag_data Simulator.env, 'dag_data) local_view
           -> ('dag_data Simulator.env, 'dag_data) node)
          Collection.entry
          option
      ; sim : (unit -> 'dag_data Simulator.state) Collection.entry
      }
      -> task

type row =
  { network : string
  ; network_description : string
  ; compute : float array
  ; protocol : string
  ; k : int
  ; protocol_description : string
  ; block_interval : float
  ; activation_delay : float
  ; number_activations : int
  ; activations : int array
  ; incentive_scheme : string
  ; incentive_scheme_description : string
  ; strategy : string
  ; strategy_description : string
  ; reward : float array
  ; ca_time : float
  ; ca_height : int
  ; machine_duration_s : float
  ; error : string
  ; version : string
  }
[@@deriving fields]

let save_rows_as_tsv filename l =
  let open Owl_dataframe in
  let df = Fields_of_row.names |> Array.of_list |> make in
  let record (row : row) =
    let string _ _ x = String x
    and float _ _ x = Float x
    and int _ _ x = Int x
    and array f _ _ arr = String (Array.to_list arr |> List.map f |> String.concat "|") in
    Fields_of_row.Direct.to_list
      row
      ~number_activations:int
      ~network:string
      ~network_description:string
      ~compute:(array string_of_float)
      ~protocol:string
      ~protocol_description:string
      ~k:int
      ~block_interval:float
      ~activation_delay:float
      ~activations:(array string_of_int)
      ~incentive_scheme:string
      ~incentive_scheme_description:string
      ~strategy:string
      ~strategy_description:string
      ~reward:(array string_of_float)
      ~ca_time:float
      ~ca_height:int
      ~machine_duration_s:float
      ~error:string
      ~version:string
    |> Array.of_list
    |> append_row df
  in
  List.iter record l;
  to_csv ~sep:'\t' df filename
;;

let prepare_row (Task { activations; network; protocol; attack; sim }) =
  let (module Protocol) = protocol in
  let strategy, strategy_description =
    match attack with
    | Some x -> x.Collection.key, x.info
    | None -> "none", ""
  in
  { network = sim.key
  ; network_description = sim.info
  ; protocol = Protocol.key
  ; protocol_description = Protocol.info
  ; k = Protocol.puzzles_per_block
  ; activation_delay = network.activation_delay
  ; number_activations = activations
  ; activations = [||]
  ; compute = [||]
  ; block_interval =
      network.activation_delay *. (Protocol.puzzles_per_block |> float_of_int)
  ; incentive_scheme = "none"
  ; incentive_scheme_description = ""
  ; strategy
  ; strategy_description
  ; reward = [||]
  ; ca_time = 0.
  ; ca_height = 0
  ; machine_duration_s = Float.nan
  ; error = ""
  ; version = Cpr_lib.version
  }
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
    let compute = Array.map (fun x -> x.Network.compute) sim.network.nodes in
    (* simulate *)
    loop ~activations:t.activations sim;
    let head = head sim in
    (* incentive stats *)
    Collection.map_to_list
      (fun rewardfn ->
        let reward = apply_reward_function ~history:Ref.history rewardfn.it sim in
        { row with
          activations = sim.activations
        ; compute
        ; incentive_scheme = rewardfn.key
        ; incentive_scheme_description = rewardfn.info
        ; reward
        ; ca_time =
            Float.Array.fold_left Float.min Float.infinity (Dag.data head).appended_at
        ; ca_height = Protocol.height (Dag.data head).value
        ; machine_duration_s = Mtime_clock.count clock |> Mtime.Span.to_s
        ; error = ""
        })
      Ref.reward_functions
  with
  | e ->
    let bt = Printexc.get_backtrace () in
    let () =
      Printf.eprintf
        "\nRUN:\tnetwork:%s\tprotocol:%s\tk:%d\tstrategy:%s\n"
        t.sim.key
        Protocol.key
        Protocol.puzzles_per_block
        (match t.attack with
        | None -> "n/a"
        | Some a -> a.key);
      Printf.eprintf "ERROR:\t%s\n%s" (Printexc.to_string e) bt;
      flush stderr
    in
    [ { row with
        error = Printexc.to_string e
      ; machine_duration_s = Mtime_clock.count clock |> Mtime.Span.to_s
      }
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
  let rows = List.concat (List.rev !acc) in
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
