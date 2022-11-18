open Cpr_lib

let run g =
  let open Rresult in
  let open ResultSyntax in
  let* net, to_graphml = Network.of_graphml g in
  let* protocol =
    match List.assoc_opt "protocol" g.data with
    | Some v -> GraphML.Data.Read.string v >>= Cpr_protocols.of_key
    | None -> Ok Cpr_protocols.nakamoto
  and* activations = GraphML.Data.Read.(get int "activations") g.data in
  let () =
    match List.assoc_opt "seed" g.data with
    | Some x -> Random.init (Hashtbl.hash x)
    | None -> Random.self_init ()
  in
  let (Protocol protocol) = protocol in
  let (module P) = protocol in
  let clock = Mtime_clock.counter () in
  let env = Simulator.(init protocol net) in
  let (module Ref) = env.referee in
  let () = Simulator.loop ~activations env in
  let machine_duration = Mtime_clock.count clock |> Mtime.Span.to_s in
  let head =
    Array.to_list env.nodes
    |> List.map (fun (Simulator.Node x) -> x.preferred x.state)
    |> Ref.winner
  in
  let headd = Dag.data head in
  let open GraphML.Data.Write in
  let graph_data =
    [ "version", string version
    ; "protocol", string P.key
    ; "protocol_description", string P.description
    ; "head_time", float (Simulator.timestamp headd)
    ; "head_progress", float (Ref.progress head)
    ; "machine_duration", float machine_duration
    ]
  in
  let node_data i =
    [ "reward", float (Float.Array.get headd.rewards i)
    ; "activations", int env.activations.(i)
    ]
  in
  Ok (to_graphml ~node_data ~graph_data ())
;;

let () = GraphML.pipe_graph run |> Rresult.R.failwith_error_msg
