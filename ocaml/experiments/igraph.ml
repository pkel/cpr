open Bos_setup
open Common
open Cpr_lib

let relativize p = Fpath.relativize ~root p |> Option.value ~default:p
let inpdir = Fpath.(root / "data" / "networks" / "input")
let outdir = Fpath.(root / "data" / "networks" / "output")
let _ = OS.Dir.create ~path:true outdir |> R.failwith_error_msg

let protocols =
  let open Simulator in
  let open Cpr_protocols in
  Protocol Nakamoto.protocol
  :: List.concat_map
       (fun k ->
         [ Protocol (B_k.protocol ~k)
         ; Protocol (B_k_lessleader.protocol ~k)
         ; Protocol (George.protocol ~k)
         ])
       [ 1; 2; 4; 8; 16; 32; 64 ]
;;

let run ~activations ~srcfile ~protocol =
  let (Simulator.Protocol protocol) = protocol in
  let open StrResult.Syntax in
  let* g = GraphML.load_graph srcfile |> R.reword_error R.msg in
  let* net, to_graphml = Network.of_graphml g |> R.reword_error R.msg in
  let clock = Mtime_clock.counter () in
  let env = Simulator.(all_honest net protocol |> init) in
  let () = Simulator.loop ~activations env in
  let machine_duration = Mtime_clock.count clock |> Mtime.Span.to_s in
  let head =
    Array.to_seq env.nodes
    |> Seq.map (fun (Simulator.Node x) -> x.preferred x.state)
    |> Dag.common_ancestor' env.global.view
    |> Option.get
  in
  R.trap_exn
    (Collection.iter (fun rewardfn ->
         let name, ext = Fpath.(split_ext (base srcfile)) in
         let dstfile =
           Fpath.(
             outdir
             / strf
                 "%a-%s-%i-%s%s"
                 pp
                 name
                 protocol.key
                 protocol.pow_per_block
                 rewardfn.key
                 ext)
         in
         let rewards = Simulator.apply_reward_function rewardfn.it head env in
         let open GraphML.Data.Write in
         let graph_data =
           [ "activations", int activations
           ; "source_graph", relativize srcfile |> Fpath.to_string |> string
           ; "version", string version
           ; "protocol", string protocol.key
           ; "protocol_info", string protocol.info
           ; "pow_per_block", int protocol.pow_per_block
           ; "reward_function", string rewardfn.key
           ; "reward_function_info", string rewardfn.info
           ; "ca_time", float (Dag.data head).appended_at
           ; "ca_height", int (protocol.height (Dag.data head).value)
           ; "machine_duration", float machine_duration
           ]
         in
         let node_data i =
           let (Node n) = env.nodes.(i) in
           [ "reward", float rewards.(i); "activations", int n.n_activations ]
         in
         let g = to_graphml ~node_data ~graph_data () in
         GraphML.write_graph g dstfile |> Result.fold ~ok:ignore ~error:failwith))
    protocol.reward_functions
  |> R.error_exn_trap_to_msg
;;

(* TODO: Write list of output graphs to txt file *)
(* TODO: Ensure unique output file names with digest? *)

let run_all activations cores =
  let open StrResult.Syntax in
  let* networks =
    OS.Dir.contents inpdir
    >>| List.filter (fun path -> OS.File.exists path |> Result.value ~default:false)
    >>| List.sort Fpath.compare
  in
  let tasks =
    List.concat_map
      (fun network -> List.map (fun protocol -> network, protocol) protocols)
      networks
  in
  let queue = ref tasks
  and all_ok = ref true in
  Progress.with_reporter
    (Common.progress_bar (List.length tasks))
    (fun progress ->
      Parany.run
        cores
        ~demux:(Common.parany_demux_list_ref queue)
        ~work:(fun (srcfile, protocol) ->
          run ~activations ~srcfile ~protocol
          |> R.reword_error_msg (R.msgf "ERROR: %a: %s" Fpath.pp (relativize srcfile)))
        ~mux:(fun r ->
          progress 1;
          Result.fold
            ~ok:ignore
            ~error:(fun (`Msg s) ->
              all_ok := false;
              prerr_endline s)
            r));
  if !all_ok then Ok () else Rresult.R.error_msgf "some tasks failed"
;;

open Cmdliner

let main_t = Term.(const run_all $ activations $ cores |> term_result)

let info =
  let doc = "simulate protocols on iGraph networks" in
  Term.info ~doc ~version "igraph"
;;

let () = Term.exit @@ Term.eval (main_t, info)
