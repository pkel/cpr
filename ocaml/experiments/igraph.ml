open Bos_setup
open Common

(* open Cpr_lib *)

let relativize p = Fpath.relativize ~root p |> Option.value ~default:p
let inpdir = Fpath.(root / "data" / "networks" / "input")
let outdir = Fpath.(root / "data" / "networks" / "output")
let _ = OS.Dir.create ~path:true outdir |> R.failwith_error_msg

let protocols =
  let open Cpr_protocols in
  nakamoto
  :: List.concat_map
       (fun k ->
         [ bk ~k; bkll ~k ]
         @ List.map
             (fun rewards -> tailstormll ~subblock_selection:Optimal ~k ~rewards)
             Tailstormll.reward_schemes)
       [ 1; 2; 4; 8; 16; 32; 64 ]
;;

[@@@ocamlformat "wrap-comments=false"]

(*

let run ~activations ~srcfile ~protocol =
  let (Protocol protocol) = protocol in
  let open ResultSyntax in
  let* g = GraphML.load_graph srcfile in
  let* net, to_graphml = Network.of_graphml g in
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
  let dstfiles =
    Collection.map_to_list
      (fun rewardfn ->
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
        let+ () = GraphML.write_graph g dstfile in
        dstfile)
      protocol.reward_functions
  in
  List.fold_left
    (fun acc el ->
      let+ acc = acc
      and+ el = el in
      el :: acc)
    (Ok [])
    dstfiles
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
          let l =
            R.ignore_error r ~use:(fun e ->
                all_ok := false;
                R.pp_msg Fmt.stderr e;
                [])
          in
          dstfiles := l @ !dstfiles));
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
  Term.info ~doc ~version "igraph"
;;

let () = Term.exit @@ Term.eval (main_t, info)

*)
