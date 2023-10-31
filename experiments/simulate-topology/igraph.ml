open Bos_setup
open Common
open Cpr_lib

let relativize p = Fpath.relativize ~root p |> Option.value ~default:p
let inpdir = Fpath.(root / "data" / "networks" / "input")
let outdir = Fpath.(root / "data" / "networks" / "output")
let _ = OS.Dir.create ~path:true outdir |> R.failwith_error_msg

let protocols =
  let open Cpr_protocols in
  nakamoto
  :: ethereum ~incentive_scheme:`Discount
  :: List.concat_map
       (fun k ->
         List.concat_map
           (fun incentive_scheme ->
             [ bk ~k ~incentive_scheme; spar ~k ~incentive_scheme ])
           [ `Block; `Constant ]
         @ List.concat_map
             (fun incentive_scheme ->
               let subblock_selection = if k > 8 then `Heuristic else `Optimal in
               [ stree ~subblock_selection ~incentive_scheme ~k
               ; tailstorm ~subblock_selection ~incentive_scheme ~k
               ])
             [ `Constant; `Discount ])
       [ 1; 2; 4; 8; 16; 32; 64 ]
;;

let run ~activations ~srcfile ~protocol =
  let (Protocol protocol) = protocol in
  let (module P) = protocol in
  let open ResultSyntax in
  let* g = GraphML.load_graph srcfile in
  let* net, to_graphml = Network.of_graphml g in
  let clock = Mtime_clock.counter () in
  let sim = Simulator.(init protocol net) in
  let (module Ref) = sim.referee in
  let () = Simulator.loop ~activations sim in
  let machine_duration = Mtime_clock.count clock |> Mtime.Span.to_s in
  let head = Simulator.head sim in
  let headd = Dag.data head in
  let name, ext = Fpath.(split_ext (base srcfile)) in
  let dstfile = Fpath.(outdir / strf "%a-%s-%s" pp name P.key ext) in
  let open GraphML.Data.Write in
  let graph_data =
    [ "activations", int activations
    ; "source_graph", relativize srcfile |> Fpath.to_string |> string
    ; "version", string version
    ; "protocol", string P.key
    ; "protocol_description", string P.description
    ; "head_time", float (Simulator.timestamp headd)
    ; "head_progress", float (Ref.progress head)
    ; "machine_duration", float machine_duration
    ]
  in
  let node_data i =
    [ "reward", float (Float.Array.get headd.rewards i)
    ; "activations", int sim.activations.(i)
    ]
  in
  let g = to_graphml ~node_data ~graph_data () in
  let+ () = GraphML.write_graph g dstfile in
  dstfile
;;

(* TODO: Ensure unique output file names with digest? *)

let run_all activations cores =
  let open ResultSyntax in
  (* one task per combination of network file and protocol *)
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
  (* process the input files in parallel *)
  let queue = ref tasks
  and dstfiles = ref []
  and all_ok = ref true in
  Progress.with_reporter
    (Common.progress_bar (List.length tasks))
    (fun progress ->
      Parany.run
        cores
        ~demux:(Common.parany_demux_list_ref queue)
        ~work:(fun (srcfile, protocol) ->
          R.trap_exn (fun () -> run ~activations ~srcfile ~protocol) ()
          |> R.error_exn_trap_to_msg
          |> R.join
          |> R.reword_error_msg (R.msgf "ERROR: %a: %s" Fpath.pp (relativize srcfile)))
        ~mux:(fun r ->
          progress 1;
          R.map (fun df -> dstfiles := df :: !dstfiles) r
          |> R.ignore_error ~use:(fun e ->
                 all_ok := false;
                 R.pp_msg Fmt.stderr e)));
  (* write recent output files to file *)
  let* () =
    OS.File.with_oc
      Fpath.(outdir / "last-run.txt")
      (fun oc () ->
        List.rev !dstfiles
        |> List.iter (fun f ->
               let rel =
                 Fpath.relativize ~root:outdir f
                 |> Option.value ~default:(relativize f)
                 |> Fpath.to_string
               in
               Printf.fprintf oc "%s\n" rel)
        |> R.ok)
      ()
    |> R.failwith_error_msg
  in
  (* return error code if something failed before *)
  if !all_ok then Ok () else R.error_msgf "some tasks failed"
;;

open Cmdliner

let main_t = Term.(const run_all $ activations $ cores |> term_result)

let info =
  let doc = "simulate protocols on iGraph networks" in
  Cmd.info ~doc ~version "igraph"
;;

let () = Cmd.v info main_t |> Cmd.eval |> Stdlib.exit
