open Cpr_lib
open Models

let fpaths ~alpha task =
  let open Fpath in
  List.map
    (fun is ->
      let a =
        Printf.sprintf
          "%s:%s:α=%.2f:%s:k=%d:%s:%s.dot"
          (tag_network task.network)
          (tag_scenario task.scenario)
          alpha
          (protocol_family task.protocol)
          (pow_per_block task.protocol)
          (tag_incentive_scheme is)
          (tag_strategy task.strategy)
      in
      v a)
    task.incentive_schemes
;;

let tasks =
  List.concat_map
    (fun alpha ->
      List.map
        (fun (protocol, incentive_schemes, strategy, activations) ->
          let t =
            { network = TwoAgentsZero { alpha }
            ; incentive_schemes
            ; protocol
            ; scenario = FirstSelfish
            ; strategy
            ; activations
            ; activation_delay = 1.
            }
          in
          fpaths ~alpha t, t)
        [ Nakamoto, [ Constant ], Honest, 10
        ; B_k_lessleadership { k = 16 }, [ Constant ], Honest, 200
        ; B_k_lessleadership { k = 8 }, [ Constant ], Honest, 100
        ; B_k_lessleadership { k = 4 }, [ Constant ], Honest, 50
        ; B_k_lessleadership { k = 16 }, [ Constant ], SelfishSimple, 200
        ; B_k_lessleadership { k = 8 }, [ Constant ], SelfishSimple, 100
        ; B_k_lessleadership { k = 4 }, [ Constant ], SelfishSimple, 50
        ; George { k = 16 }, [ Constant ], Honest, 48
        ; George { k = 8 }, [ Constant ], Honest, 24
        ; George { k = 4 }, [ Constant ], Honest, 12
        ])
    [ 0.25; 0.33; 0.5 ]
;;

let print_dag oc (sim, rewards) =
  let open Simulator in
  let node_attr n =
    let open Simulator in
    let d = Dag.data n in
    [ ( "label"
      , Printf.sprintf
          "%s | t:%.1f | r:%.2g"
          (match d.appended_by with
          | None -> "genesis"
          | Some 0 -> "a"
          | Some _ -> "d")
          d.appended_at
          rewards.(Dag.id n) )
    ; ( "color"
      , match Dag.children sim.global_view n with
        | [] -> "red"
        | _ -> "black" )
    ]
  in
  Dag.dot oc sim.global_view ~node_attr (Dag.roots sim.dag) |> Result.ok
;;

let run (fpaths, task) =
  let (S s) = setup task in
  (* simulate *)
  let open Simulator in
  init ?deviations:s.deviations s.params s.protocol
  |> loop s.params
  |> fun sim ->
  List.iter2
    (fun path rw ->
      let rewards = Array.make (Dag.size sim.dag) 0. in
      let () =
        rw
          sim.global_view
          (fun n -> n.value)
          (fun x n -> rewards.(Dag.id n) <- rewards.(Dag.id n) +. x)
          (let (SNode n) = sim.nodes.(0) in
           n.preferred n.state)
      in
      let path =
        let open Fpath in
        v "." / "fig" / "chains" // path
      in
      let open Bos.OS in
      let d = Dir.create ~path:true (Fpath.parent path) in
      Result.bind d (fun _ -> File.with_oc path print_dag (sim, rewards))
      |> Result.join
      |> Rresult.R.failwith_error_msg)
    fpaths
    s.reward_functions
;;

let () = List.iter run tasks
