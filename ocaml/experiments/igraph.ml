open Bos_setup
open Cpr_lib

let root =
  Cmd.(v "git" % "rev-parse" % "--show-toplevel")
  |> OS.Cmd.run_out
  |> OS.Cmd.to_string
  >>= Fpath.of_string
  |> R.failwith_error_msg
;;

let version =
  Cmd.(v "git" % "describe" % "--tags" % "--dirty")
  |> OS.Cmd.run_out
  |> OS.Cmd.to_string
  |> R.failwith_error_msg
;;

let relativize p = Fpath.relativize ~root p |> Option.value ~default:p
let inpdir = Fpath.(root / "data" / "networks" / "input")
let outdir = Fpath.(root / "data" / "networks" / "output")
let _ = OS.Dir.create ~path:true outdir |> R.failwith_error_msg
let activations = 100000
let protocol = Cpr_protocols.Nakamoto.protocol

let run ~srcfile =
  let open StrResult.Syntax in
  let* g = GraphML.load_graph srcfile |> R.reword_error R.msg in
  let* net, to_graphml = Network.of_graphml g |> R.reword_error R.msg in
  let env = Simulator.(all_honest net protocol |> init) in
  let () = Simulator.loop ~activations env in
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
           Fpath.(outdir / strf "%a-%s-%s%s" pp name protocol.key rewardfn.key ext)
         in
         let rewards = Simulator.apply_reward_function rewardfn.it head env in
         let open GraphML.Data.Write in
         let graph_data =
           (* TODO add missing fields from Csv_runner *)
           [ "activations", int activations
           ; "source_graph", relativize srcfile |> Fpath.to_string |> string
           ; "version", string version
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

let () =
  OS.Dir.contents inpdir
  >>| List.filter (fun path -> OS.File.exists path |> Result.value ~default:false)
  >>| List.sort Fpath.compare
  >>| List.iter (fun srcfile ->
          run ~srcfile
          |> R.reword_error_msg (relativize srcfile |> R.msgf "ERROR: %a: %s" Fpath.pp)
          |> Result.fold ~ok:ignore ~error:(fun (`Msg s) -> prerr_endline s))
  |> R.failwith_error_msg
;;
