open Cpr_lib
open Models

let fpath (Csv_runner.Task t) ~rewardfn =
  let l =
    let open Collection in
    [ t.sim.key
    ; Printf.sprintf "d=%g" t.network.activation_delay
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
  [ "Network", t.sim.info
  ; "Activation Delay", string_of_float t.network.activation_delay
  ; "Protocol", t.protocol.info
  ; "PoW per Block (k)", string_of_int t.protocol.pow_per_block
  ; "Incentive Scheme", rewardfn.info
  ]
  @
  match t.attack with
  | Some x -> [ "Attack Strategy", x.info ]
  | None -> []
;;

(* TODO : this should be part of the scenario *)
let node_name (Csv_runner.Task t) =
  if Array.length (t.sim.it ()).nodes = 2
  then
    function
    | None -> "genesis"
    | Some 0 -> "a"
    | Some _ -> "d"
  else
    function
    | None -> "genesis"
    | Some i -> "n" ^ string_of_int i
;;

let tasks =
  List.concat_map
    (fun (P protocol, n_activations) ->
      List.map
        (fun activation_delay ->
          let sim, network = honest_clique ~activation_delay ~n:7 protocol in
          Csv_runner.Task
            { activations = n_activations; protocol; attack = None; sim; network })
        [ 2.; 4. ]
      @ List.concat_map
          (fun alpha ->
            Collection.map_to_list
              (fun attack ->
                let sim, network = two_agents ~alpha protocol attack in
                Csv_runner.Task
                  { activations = n_activations
                  ; protocol
                  ; attack = Some attack
                  ; sim
                  ; network
                  })
              protocol.attacks)
          [ 0.25; 0.33; 0.5 ])
    [ nakamoto, 30
    ; bk ~k:8, 100
    ; bk ~k:4, 50
    ; bk ~k:1, 20
    ; bk_lessleader ~k:8, 100
    ; bk_lessleader ~k:4, 50
    ; bk_lessleader ~k:1, 20
    ; tailstorm ~k:8, 100
    ; tailstorm ~k:4, 50
    ; tailstorm ~k:1, 20
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
  (* simulate *)
  let open Simulator in
  let env = t.sim.it () in
  loop ~activations:t.activations env;
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
