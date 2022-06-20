open Cpr_lib.Next
open Models

let fpath (Csv_runner.Task t) ~rewardfn =
  let (module Protocol) = t.protocol in
  let l =
    let open Collection in
    [ t.sim.key
    ; Printf.sprintf "d=%g" t.network.activation_delay
    ; Protocol.key
    ; Printf.sprintf "k=%i" Protocol.puzzles_per_block
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
  let (module Protocol) = t.protocol in
  let open Collection in
  [ "Network", t.sim.info
  ; "Activation Delay", string_of_float t.network.activation_delay
  ; "Protocol", Protocol.info
  ; "PoW per Block (k)", string_of_int Protocol.puzzles_per_block
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

let tasks_per_attack_space (AttackSpace (module A)) n_activations =
  let protocol = (module A.Protocol : Protocol with type data = _) in
  List.map
    (fun activation_delay ->
      let sim, network = honest_clique ~activation_delay ~n:7 protocol in
      Csv_runner.Task
        { activations = n_activations; protocol; attack = None; sim; network })
    [ 2.; 4. ]
  @ List.concat_map
      (fun alpha ->
        Collection.map_to_list
          (fun policy ->
            let attack =
              Collection.
                { key = A.key ^ "-" ^ policy.key
                ; info = A.info ^ "; " ^ policy.info
                ; it = A.attacker policy.it
                }
            in
            let sim, network = two_agents ~alpha protocol attack in
            Csv_runner.Task
              { activations = n_activations
              ; protocol
              ; attack = Some attack
              ; sim
              ; network
              })
          A.policies)
      [ 0.25; 0.33; 0.5 ]
;;

let tasks =
  let open Cpr_protocols in
  List.concat
    [ tasks_per_attack_space nakamoto_ssz 30
    ; tasks_per_attack_space (bk_ssz ~k:8) 100
    ; tasks_per_attack_space (bk_ssz ~k:4) 50
    ; tasks_per_attack_space (bk_ssz ~k:1) 20
    ; tasks_per_attack_space (bkll_ssz ~k:8) 100
    ; tasks_per_attack_space (bkll_ssz ~k:4) 50
    ; tasks_per_attack_space (bkll_ssz ~k:1) 20
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
    sim.global_view
    ~node_attr
    (Dag.roots sim.dag)
  |> Result.ok
;;

let run (Csv_runner.Task t) =
  (* simulate *)
  let open Simulator in
  let (module Protocol) = t.protocol in
  let env = t.sim.it () in
  let (module Ref) = env.referee in
  loop ~activations:t.activations env;
  let confirmed = Array.make (Dag.size env.dag) false in
  Collection.iter
    (fun rewardfn ->
      let rewards = Array.make (Dag.size env.dag) 0. in
      let () =
        Seq.iter
          (fun n ->
            confirmed.(Dag.id n) <- true;
            rewardfn.it
              ~assign:(fun x n -> rewards.(Dag.id n) <- rewards.(Dag.id n) +. x)
              n)
          (Dag.iterate_ancestors env.global_view [ head env ])
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
            , Protocol.describe
            , node_name (Task t) ))
      |> Result.join
      |> Rresult.R.failwith_error_msg)
    Ref.reward_functions
;;

let () =
  Random.self_init ();
  List.iter run tasks
;;
