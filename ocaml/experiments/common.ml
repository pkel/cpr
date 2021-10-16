open Bos_setup

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

let progress_bar n =
  let open Progress.Line in
  list [ brackets (elapsed ()); bar n; count_to n; parens (const "eta: " ++ eta n) ]
;;

let parany_demux_list_ref queue () =
  match !queue with
  | [] -> raise Parany.End_of_input
  | hd :: tl ->
    queue := tl;
    hd
;;

let activations =
  let open Cmdliner in
  let doc = "Number of proof-of-work activations simulated per output row." in
  let env = Arg.env_var "CPR_ACTIVATIONS" ~doc in
  Arg.(value & opt int 10000 & info [ "n"; "activations" ] ~env ~docv:"ACTIVATIONS" ~doc)
;;

let cores =
  let open Cmdliner in
  let doc = "Number of simulation tasks run in parallel" in
  let env = Arg.env_var "CPR_CORES" ~doc in
  Arg.(value & opt int (Cpu.numcores ()) & info [ "p"; "cores" ] ~env ~docv:"CORES" ~doc)
;;
