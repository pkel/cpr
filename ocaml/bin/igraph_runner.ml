open Cpr_lib

let run g =
  let open Rresult in
  let open ResultSyntax in
  let* net, to_graphml = Network.of_graphml g in
  let* protocol_s =
    match List.assoc_opt "protocol" g.data with
    | Some v -> GraphML.Data.Read.string v >>= Cpr_protocols.Serializable.of_string
    | None -> Ok Nakamoto
  and* activations = GraphML.Data.Read.(get int "activations") g.data in
  let (Protocol protocol) = Cpr_protocols.Serializable.to_protocol protocol_s in
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
  let* rewardfn_s =
    match List.assoc_opt "reward_function" g.data with
    | Some v -> GraphML.Data.Read.string v
    | None ->
      let ks = Collection.keys Ref.reward_functions in
      List.nth_opt ks 0
      |> Rresult.R.of_option ~none:(fun () ->
             R.error_msgf "empty reward functions collections for protocol %s" P.key)
  in
  let* rewardfn =
    Collection.get rewardfn_s Ref.reward_functions
    |> R.of_option ~none:(fun () ->
           R.error_msgf "reward function %s not found for protocol %s" rewardfn_s P.key)
  in
  let rewards = Simulator.apply_reward_function ~history:Ref.history rewardfn.it env in
  let open GraphML.Data.Write in
  let graph_data =
    [ "version", string version
    ; "protocol", string P.key
    ; "protocol_info", string P.info
    ; "puzzles_per_block", int P.puzzles_per_block
    ; "reward_function", string rewardfn.key
    ; "reward_function_info", string rewardfn.info
    ; "head_time", float (Dag.data head).appended_at
    ; "head_height", int (P.height (Dag.data head).value)
    ; "head_progress", float (P.progress (Dag.data head).value)
    ; "machine_duration", float machine_duration
    ]
  in
  let node_data i =
    [ "reward", float rewards.(i); "activations", int env.activations.(i) ]
  in
  Ok (to_graphml ~node_data ~graph_data ())
;;

let () = GraphML.pipe_graph run |> Rresult.R.failwith_error_msg
