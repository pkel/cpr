open Cpr_lib
open Models

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
  ; scenario : string
  ; scenario_description : string
  ; strategy : string
  ; strategy_description : string
  ; reward : float array
  }

let df_spec =
  let open Owl_dataframe in
  let array f arr = String (Array.to_list arr |> List.map f |> String.concat "|") in
  [| ("network", fun row -> String row.network)
   ; ("network_description", fun row -> String row.network_description)
   ; ("compute", fun row -> array string_of_float row.compute)
   ; ("protocol", fun row -> String row.protocol)
   ; ("k", fun row -> Int row.k)
   ; ("protocol_description", fun row -> String row.protocol_description)
   ; ("block_interval", fun row -> Float row.block_interval)
   ; ("activation_delay", fun row -> Float row.activation_delay)
   ; ("number_activations", fun row -> Int row.number_activations)
   ; ("activations", fun row -> array string_of_int row.activations)
   ; ("incentive_scheme", fun row -> String row.incentive_scheme)
   ; ("incentive_scheme_description", fun row -> String row.incentive_scheme_description)
   ; ("scenario", fun row -> String row.scenario)
   ; ("scenario_description", fun row -> String row.scenario_description)
   ; ("strategy", fun row -> String row.strategy)
   ; ("strategy_description", fun row -> String row.strategy_description)
   ; ("reward", fun row -> array string_of_float row.reward)
  |]
;;

let save_rows_as_tsv filename l =
  let df = Owl_dataframe.make (Array.map fst df_spec) in
  let record (row : row) =
    Array.map (fun (_, f) -> f row) df_spec |> Owl_dataframe.append_row df
  in
  List.iter record l;
  Owl_dataframe.to_csv ~sep:'\t' df filename
;;

let run task =
  let (S s) = setup task in
  (* network stats *)
  let compute =
    let open Network in
    Array.map (fun x -> x.compute) s.network.nodes
  in
  (* simulate *)
  let open Simulator in
  init ?deviations:s.deviations s.params s.protocol
  |> loop s.params
  |> fun sim ->
  let activations = Array.map (fun (SNode x) -> x.n_activations) sim.nodes in
  Array.to_seq sim.nodes
  |> Seq.map (fun (SNode x) -> x.preferred x.state)
  |> Dag.common_ancestor' sim.global_view
  |> function
  | None -> failwith "no common ancestor found"
  | Some common_chain ->
    (* incentive stats *)
    List.map2
      (fun is rewardfn ->
        let reward = apply_reward_function rewardfn common_chain sim in
        { network = tag_network task.network
        ; network_description = describe_network task.network
        ; protocol = protocol_family task.protocol
        ; protocol_description = describe_protocol task.protocol
        ; k = pow_per_block task.protocol
        ; activation_delay = task.activation_delay
        ; number_activations = task.activations
        ; activations
        ; compute
        ; block_interval =
            task.activation_delay *. (pow_per_block task.protocol |> float_of_int)
        ; incentive_scheme = tag_incentive_scheme is
        ; incentive_scheme_description = describe_incentive_scheme is
        ; strategy = tag_strategy task.strategy
        ; strategy_description = describe_strategy task.strategy
        ; scenario = tag_scenario task.scenario
        ; scenario_description = describe_scenario task.scenario
        ; reward
        })
      task.incentive_schemes
      s.reward_functions
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
          ~work:(fun task -> run task (*TODO log start; catch and marshal error/trace *))
          ~mux:(fun l ->
            progress 1;
            acc := l :: !acc (*TODO log error *))
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
