open Cpr_lib
open Models

let tasks0 =
  let fpath ~alpha ~rewardfn task =
    let open Fpath in
    let a =
      Printf.sprintf
        "%s:%s:α=%.2f:%s:k=%d:%s:%s.dot"
        (tag_network task.network)
        (tag_scenario task.scenario)
        alpha
        (protocol_family task.protocol)
        (pow_per_block task.protocol)
        rewardfn
        (tag_strategy task.strategy)
    in
    v a
  and legend ~alpha ~rewardfn task =
    [ "Network", describe_network task.network
    ; "Scenario", describe_scenario task.scenario
    ; "Attacker Compute (α)", string_of_float alpha
    ; "Protocol", describe_protocol task.protocol
    ; "PoW per Block (k)", string_of_int (pow_per_block task.protocol)
    ; "Incentive Scheme", rewardfn
    ; "Attack Strategy", describe_strategy task.strategy
    ]
  and label_node = function
    | None -> "genesis"
    | Some 0 -> "a"
    | Some _ -> "d"
  in
  List.concat_map
    (fun alpha ->
      List.concat_map
        (fun (protocol, strategies, activations) ->
          List.map
            (fun strategy ->
              let t =
                { network = TwoAgentsZero { alpha }
                ; protocol
                ; scenario = FirstSelfish
                ; strategy
                ; activations
                ; activation_delay = 1.
                }
              in
              t, fpath ~alpha t, legend ~alpha t, label_node)
            strategies)
        [ Nakamoto, [ Honest; SelfishAdvanced; NumHonest; NumSelfishAdvanced ], 30
        ; B_k { k = 16 }, [ Honest; SelfishAdvanced; NumHonest; NumSelfishAdvanced ], 200
        ; B_k { k = 8 }, [ Honest; SelfishAdvanced; NumHonest; NumSelfishAdvanced ], 100
        ; B_k { k = 4 }, [ Honest; SelfishAdvanced; NumHonest; NumSelfishAdvanced ], 50
        ; B_k { k = 1 }, [ Honest; SelfishAdvanced; NumHonest; NumSelfishAdvanced ], 20
        ; ( B_k_lessleadership { k = 16 }
          , [ Honest; SelfishAdvanced; NumHonest; NumSelfishAdvanced ]
          , 200 )
        ; ( B_k_lessleadership { k = 8 }
          , [ Honest; SelfishAdvanced; NumHonest; NumSelfishAdvanced ]
          , 100 )
        ; ( B_k_lessleadership { k = 4 }
          , [ Honest; SelfishAdvanced; NumHonest; NumSelfishAdvanced ]
          , 50 )
        ; ( B_k_lessleadership { k = 1 }
          , [ Honest; SelfishAdvanced; NumHonest; NumSelfishAdvanced ]
          , 20 )
        ; ( George { k = 16 }
          , [ Honest; SelfishAdvanced; NumHonest; NumSelfishAdvanced ]
          , 48 )
        ; George { k = 8 }, [ Honest; SelfishAdvanced; NumHonest; NumSelfishAdvanced ], 24
        ; George { k = 4 }, [ Honest; SelfishAdvanced; NumHonest; NumSelfishAdvanced ], 12
        ])
    [ 0.25; 0.33; 0.5 ]
;;

let tasks1 =
  let fpath ~activation_delay ~rewardfn task =
    let open Fpath in
    let a =
      Printf.sprintf
        "%s:%s:d=%.2g:%s:k=%d:%s.dot"
        (tag_network task.network)
        (tag_scenario task.scenario)
        activation_delay
        (protocol_family task.protocol)
        (pow_per_block task.protocol)
        rewardfn
    in
    v a
  and legend ~activation_delay ~rewardfn task =
    [ "Network", describe_network task.network
    ; "Scenario", describe_scenario task.scenario
    ; "Activation Delay", string_of_float activation_delay
    ; "Protocol", describe_protocol task.protocol
    ; "PoW per Block (k)", string_of_int (pow_per_block task.protocol)
    ; "Incentive Scheme", rewardfn
    ]
  and label_node = function
    | Some n -> "n:" ^ string_of_int n
    | None -> "genesis"
  in
  List.concat_map
    (fun activation_delay ->
      List.map
        (fun (protocol, activations) ->
          let t =
            { network = CliqueUniform10
            ; protocol
            ; scenario = AllHonest
            ; strategy = Honest
            ; activations
            ; activation_delay
            }
          in
          t, fpath ~activation_delay t, legend ~activation_delay t, label_node)
        [ Nakamoto, 10
        ; B_k { k = 16 }, 200
        ; B_k { k = 8 }, 100
        ; B_k { k = 4 }, 50
        ; B_k_lessleadership { k = 16 }, 200
        ; B_k_lessleadership { k = 8 }, 100
        ; B_k_lessleadership { k = 4 }, 50
        ; George { k = 16 }, 100
        ; George { k = 8 }, 50
        ; George { k = 4 }, 30
        ])
    [ 1.; 2.; 4. ]
;;

let print_dag oc (sim, confirmed, rewards, legend, label_vtx, label_node) =
  let open Simulator in
  let node_attr n =
    let open Simulator in
    let d = Dag.data n in
    [ ( "label"
      , Printf.sprintf
          "%s | %s | t:%.1f%s | r:%.2g"
          (label_vtx d.value)
          (label_node d.appended_by)
          d.appended_at
          (if d.released_at <> d.appended_at
          then Printf.sprintf "-%.1f" d.released_at
          else "")
          rewards.(Dag.id n) )
    ; ("color", if confirmed.(Dag.id n) then "black" else "red")
    ]
  in
  Dag.dot
    (Format.formatter_of_out_channel oc)
    ~legend
    sim.global.view
    ~node_attr
    (Dag.roots sim.dag)
  |> Result.ok
;;

let run (task, fpath, legend, label_node) =
  let (S s) = setup task in
  (* simulate *)
  let open Simulator in
  let env =
    let x = all_honest s.params s.protocol in
    List.iter (fun (node, Deviation d) -> patch ~node d x |> ignore) s.deviations;
    init x
  in
  loop s.params env;
  let head =
    Array.to_seq env.nodes
    |> Seq.map (fun (Node x) -> x.preferred x.state)
    |> Dag.common_ancestor' env.global.view
    |> Option.get
  in
  let confirmed = Array.make (Dag.size env.dag) false in
  Collection.iter
    (fun ~info key rewardfn ->
      let rewards = Array.make (Dag.size env.dag) 0. in
      let () =
        Seq.iter
          (fun n ->
            confirmed.(Dag.id n) <- true;
            rewardfn
              ~view:env.global
              ~assign:(fun x n -> rewards.(Dag.id n) <- rewards.(Dag.id n) +. x)
              n)
          (Dag.iterate_ancestors env.global.view [ head ])
      in
      let path =
        let open Fpath in
        v "." / "fig" / "chains" // fpath ~rewardfn:key
      in
      let open Bos.OS in
      let d = Dir.create ~path:true (Fpath.parent path) in
      Result.bind d (fun _ ->
          File.with_oc
            path
            print_dag
            ( env
            , confirmed
            , rewards
            , legend ~rewardfn:info
            , s.protocol.describe
            , label_node ))
      |> Result.join
      |> Rresult.R.failwith_error_msg)
    s.protocol.reward_functions
;;

let () =
  Random.self_init ();
  List.iter run (tasks0 @ tasks1)
;;
