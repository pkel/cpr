open Cpr_lib

type task =
  | Task :
      { params : Simulator.params
      ; protocol : ('dag_data Simulator.data, 'dag_data, Simulator.pow, 'state) protocol
      ; attack :
          ('dag_data Simulator.data, 'dag_data, Simulator.pow) opaque_node
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
    |> Array.of_list
    |> append_row df
  in
  List.iter record l;
  to_csv ~sep:'\t' df filename
;;

let prepare_row (Task { params; protocol; attack; sim }) =
  let strategy, strategy_description =
    match attack with
    | Some x -> x.Collection.key, x.info
    | None -> "none", ""
  in
  { network = sim.key
  ; network_description = sim.info
  ; protocol = protocol.key
  ; protocol_description = protocol.info
  ; k = protocol.pow_per_block
  ; activation_delay = params.activation_delay
  ; number_activations = params.activations
  ; activations = [||]
  ; compute = [||]
  ; block_interval = params.activation_delay *. (protocol.pow_per_block |> float_of_int)
  ; incentive_scheme = "none"
  ; incentive_scheme_description = ""
  ; strategy
  ; strategy_description
  ; reward = [||]
  ; ca_time = 0.
  ; ca_height = 0
  ; machine_duration_s = Float.nan
  ; error = ""
  }
;;

let run task =
  let row = prepare_row task in
  let (Task t) = task in
  let clock = Mtime_clock.counter () in
  try
    let open Simulator in
    let sim = t.sim.it () in
    let compute = Array.map (fun x -> x.Network.compute) sim.network.nodes in
    (* simulate *)
    loop t.params sim;
    let activations = Array.map (fun (Node x) -> x.n_activations) sim.nodes in
    Array.to_seq sim.nodes
    |> Seq.map (fun (Node x) -> x.preferred x.state)
    |> Dag.common_ancestor' sim.global.view
    |> function
    | None -> failwith "no common ancestor found"
    | Some common_chain ->
      (* incentive stats *)
      Collection.map_to_list
        (fun rewardfn ->
          let reward = apply_reward_function rewardfn.it common_chain sim in
          { row with
            activations
          ; compute
          ; incentive_scheme = rewardfn.key
          ; incentive_scheme_description = rewardfn.info
          ; reward
          ; ca_time = (Dag.data common_chain).appended_at
          ; ca_height = t.protocol.height (Dag.data common_chain).value
          ; machine_duration_s = Mtime_clock.count clock |> Mtime.Span.to_s
          ; error = ""
          })
        t.protocol.reward_functions
  with
  | e ->
    let bt = Printexc.get_backtrace () in
    let () =
      Printf.eprintf
        "\nRUN:\tnetwork:%s\tprotocol:%s\tk:%d\tstrategy:%s\n"
        t.sim.key
        t.protocol.key
        t.protocol.pow_per_block
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

let bar ~n_tasks =
  let open Progress.Line in
  list
    [ brackets (elapsed ())
    ; bar n_tasks
    ; count_to n_tasks
    ; parens (const "eta: " ++ eta n_tasks)
    ]
;;

let main tasks n_activations n_cores filename =
  let tasks = tasks ~n_activations in
  let n_tasks = List.length tasks in
  let queue = ref tasks in
  let acc = ref [] in
  Printf.eprintf "Run %d simulations in parallel\n" n_cores;
  Progress.with_reporter (bar ~n_tasks) (fun progress ->
      if n_cores > 1
      then
        Parany.run
          n_cores
          ~demux:(fun () ->
            match !queue with
            | [] -> raise Parany.End_of_input
            | hd :: tl ->
              queue := tl;
              hd)
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

let activations =
  let doc = "Number of proof-of-work activations simulated per output row." in
  let env = Arg.env_var "CPR_ACTIVATIONS" ~doc in
  Arg.(value & opt int 10000 & info [ "n"; "activations" ] ~env ~docv:"ACTIVATIONS" ~doc)
;;

let cores =
  let doc = "Number of simulation tasks run in parallel" in
  let env = Arg.env_var "CPR_CORES" ~doc in
  Arg.(value & opt int (Cpu.numcores ()) & info [ "p"; "cores" ] ~env ~docv:"CORES" ~doc)
;;

let filename =
  let doc = "Name of CSV output file (tab-separated)." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"OUTPUT" ~doc)
;;

let main_t tasks = Term.(const (main tasks) $ activations $ cores $ filename)

let info =
  let doc = "simulate withholding attacks against various protocols" in
  Term.info "withholding" ~doc
;;

let command tasks = Term.exit @@ Term.eval (main_t tasks, info)
