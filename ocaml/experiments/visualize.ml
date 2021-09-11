open Cpr_lib
open Models

let fpath (Csv_runner.Task t) ~rewardfn =
  let l =
    let open Collection in
    [ tag_network t.model.network
    ; Printf.sprintf "d=%g" t.model.activation_delay
    ; (match t.model.network, t.model.scenario with
      | TwoAgentsZero { alpha }, FirstSelfish -> Printf.sprintf "α=%.2f" alpha
      | _ -> tag_scenario t.model.scenario)
    ; t.protocol.key
    ; Printf.sprintf "k=%i" t.protocol.pow_per_block
    ; rewardfn.key
    ]
    @
    match t.attack with
    | Some x -> [ x.key ]
    | None -> []
  in
  Fpath.v (String.concat ":" l ^ ".dot")
;;

let legend (Csv_runner.Task t) ~rewardfn =
  let open Collection in
  [ "Network", describe_network t.model.network
  ; "Activation Delay", string_of_float t.model.activation_delay
  ; (match t.model.network, t.model.scenario with
    | TwoAgentsZero { alpha }, FirstSelfish ->
      "Attacker Compute (α)", string_of_float alpha
    | _ -> "Scenario", describe_scenario t.model.scenario)
  ; "Protocol", t.protocol.info
  ; "PoW per Block (k)", string_of_int t.protocol.pow_per_block
  ; "Incentive Scheme", rewardfn.info
  ]
  @
  match t.attack with
  | Some x -> [ "Attack Strategy", x.info ]
  | None -> []
;;

let node_name (Csv_runner.Task t) =
  match t.model.network with
  | TwoAgentsZero _ ->
    (function
    | None -> "genesis"
    | Some 0 -> "a"
    | Some _ -> "d")
  | _ ->
    (function
    | None -> "genesis"
    | Some i -> "n" ^ string_of_int i)
;;

let attack_model ~n_activations ~alpha =
  { network = TwoAgentsZero { alpha }
  ; scenario = FirstSelfish
  ; activations = n_activations
  ; activation_delay = 1.
  }
;;

let honest_model ~n_activations ~activation_delay =
  { network = CliqueUniform10
  ; scenario = AllHonest
  ; activations = n_activations
  ; activation_delay
  }
;;

let tasks =
  List.concat_map
    (fun (P protocol, n_activations) ->
      List.concat_map
        (fun activation_delay ->
          let model = honest_model ~n_activations ~activation_delay in
          let open Csv_runner in
          [ Task { model; protocol; attack = None } ])
        [ 1.; 2.; 4. ]
      @ List.concat_map
          (fun alpha ->
            let model = attack_model ~n_activations ~alpha in
            let open Csv_runner in
            Task { model; protocol; attack = None }
            :: Collection.map_to_list
                 (fun attack -> Task { model; protocol; attack = Some attack })
                 protocol.attacks)
          [ 0.25; 0.33; 0.5 ])
    [ nakamoto, 30
    ; bk ~k:16, 200
    ; bk ~k:8, 100
    ; bk ~k:4, 50
    ; bk ~k:1, 20
    ; bk_lessleader ~k:16, 200
    ; bk_lessleader ~k:8, 100
    ; bk_lessleader ~k:4, 50
    ; bk_lessleader ~k:1, 20
    ; george ~k:16, 200
    ; george ~k:8, 100
    ; george ~k:4, 50
    ; george ~k:1, 20
    ]
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

let run (Csv_runner.Task t) =
  let s = setup t.model in
  (* simulate *)
  let open Simulator in
  let env =
    let x = all_honest s.params t.protocol in
    let () =
      match t.attack, s.attacker with
      (* TODO argument to patch should be opaque_node *)
      | Some { it = Node d; _ }, Some node -> patch ~node d x |> ignore
      | _ -> ()
    in
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
    (fun rewardfn ->
      let rewards = Array.make (Dag.size env.dag) 0. in
      let () =
        Seq.iter
          (fun n ->
            confirmed.(Dag.id n) <- true;
            rewardfn.it
              ~view:env.global
              ~assign:(fun x n -> rewards.(Dag.id n) <- rewards.(Dag.id n) +. x)
              n)
          (Dag.iterate_ancestors env.global.view [ head ])
      in
      let path =
        let open Fpath in
        v "." / "fig" / "chains" // fpath (Task t) ~rewardfn
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
            , legend (Task t) ~rewardfn
            , t.protocol.describe
            , node_name (Task t) ))
      |> Result.join
      |> Rresult.R.failwith_error_msg)
    t.protocol.reward_functions
;;

let () =
  Random.self_init ();
  List.iter run tasks
;;
