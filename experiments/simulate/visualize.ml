open Cpr_lib
open Models

let fpath (Csv_runner.Task t) =
  let (module Protocol) = t.protocol in
  let l =
    let open Collection in
    [ t.sim.key; Printf.sprintf "d=%g" t.network.activation_delay; Protocol.key ]
    @
    match t.attack with
    | Some x -> [ x.key; Printf.sprintf "alpha=%g" t.network.nodes.(0).compute ]
    | None -> []
  in
  Fpath.v (String.concat ":" l ^ ".dot")
;;

let legend (Csv_runner.Task t) =
  let (module Protocol) = t.protocol in
  let open Collection in
  [ "Network", t.sim.info
  ; "Activation Delay", string_of_float t.network.activation_delay
  ; "Protocol", Protocol.description
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

let tasks_per_protocol f n_activations incentive_scheme =
  let (Protocol (module P)) = f ~incentive_scheme in
  List.map
    (fun activation_delay ->
      let sim, network = honest_clique ~activation_delay ~n:7 (module P) in
      Csv_runner.Task
        { activations = n_activations
        ; protocol = (module P)
        ; attack = None
        ; sim
        ; network
        })
    [ 1.; 2.; 4. ]
;;

let tasks_per_protocol f i = List.concat_map (tasks_per_protocol f i)

let tasks_per_attack_space f n_activations incentive_scheme =
  let (AttackSpace (module A)) = f ~incentive_scheme in
  let protocol = (module A.Protocol : Protocol with type data = _) in
  List.map
    (fun activation_delay ->
      let sim, network = honest_clique ~activation_delay ~n:7 protocol in
      Csv_runner.Task
        { activations = n_activations; protocol; attack = None; sim; network })
    [ 1.; 2.; 4. ]
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
            let sim, network = selfish_mining ~defenders:10 ~alpha 0.9 protocol attack in
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

let tasks_per_attack_space f i = List.concat_map (tasks_per_attack_space f i)

let tasks =
  let open Cpr_protocols in
  let tailstorm = tailstorm ~subblock_selection:`Optimal
  and tailstorm_ssz = tailstorm_ssz ~subblock_selection:`Optimal
  and tailstormll = tailstormll ~subblock_selection:`Optimal
  and tailstormll_ssz = tailstormll_ssz ~subblock_selection:`Optimal
  and nakamoto_ssz ~incentive_scheme:_ = nakamoto_ssz in
  List.concat
    [ tasks_per_attack_space nakamoto_ssz 30 [ `Dummy ]
    ; tasks_per_attack_space ethereum_ssz 30 [ `Discount ]
    ; tasks_per_attack_space (bk_ssz ~k:8) 100 [ `Constant ]
    ; tasks_per_attack_space (bk_ssz ~k:4) 50 [ `Constant ]
    ; tasks_per_attack_space (bk_ssz ~k:1) 20 [ `Constant ]
    ; tasks_per_attack_space (bkll_ssz ~k:8) 100 [ `Constant ]
    ; tasks_per_attack_space (bkll_ssz ~k:4) 50 [ `Constant ]
    ; tasks_per_attack_space (bkll_ssz ~k:1) 20 [ `Constant ]
    ; tasks_per_attack_space (tailstorm_ssz ~k:8) 50 [ `Constant; `Discount ]
    ; tasks_per_attack_space (tailstorm_ssz ~k:4) 25 [ `Constant; `Discount ]
    ; tasks_per_attack_space (tailstorm_ssz ~k:1) 20 [ `Constant; `Discount ]
    ; tasks_per_attack_space (tailstormll_ssz ~k:8) 50 [ `Constant; `Discount ]
    ; tasks_per_attack_space (tailstormll_ssz ~k:4) 25 [ `Constant; `Discount ]
    ; tasks_per_attack_space (tailstormll_ssz ~k:1) 20 [ `Constant; `Discount ]
    ; tasks_per_protocol (tailstorm ~k:8) 50 [ `Constant; `Discount ]
    ; tasks_per_protocol (tailstorm ~k:4) 25 [ `Constant; `Discount ]
    ; tasks_per_protocol (tailstorm ~k:1) 20 [ `Constant; `Discount ]
    ; tasks_per_protocol (tailstormll ~k:8) 50 [ `Constant; `Discount ]
    ; tasks_per_protocol (tailstormll ~k:4) 25 [ `Constant; `Discount ]
    ; tasks_per_protocol (tailstormll ~k:1) 20 [ `Constant; `Discount ]
    ]
;;

let print_dag (type a) oc (sim, confirmed, rewards, legend, vtx_info) =
  let open Simulator in
  let reward n = List.fold_left (fun s (_, x) -> s +. x) 0. rewards.(Dag.id n) in
  let node_attr n =
    let open Simulator in
    [ ( "label"
      , debug_info ~info:vtx_info n @ [ "reward", Printf.sprintf "%.2f" (reward n) ]
        |> List.map (function
               | "", s | s, "" -> s
               | k, v -> k ^ ": " ^ v)
        |> String.concat "\\n" )
    ; ("color", if confirmed.(Dag.id n) then "black" else "red")
    ]
  in
  let (module Ref : Referee with type block = _ and type data = a) = sim.referee in
  let module Tools = Dagtools.Make (Ref.Block) in
  Tools.dot (Format.formatter_of_out_channel oc) ~legend ~node_attr (Dag.roots sim.dag)
  |> Result.ok
;;

let run (Csv_runner.Task t) =
  (* simulate *)
  let open Simulator in
  let (module Protocol) = t.protocol in
  let sim = t.sim.it () in
  let (module Ref) = sim.referee in
  loop ~activations:t.activations sim;
  let confirmed = Array.make (Dag.size sim.dag) false in
  let rewards = Array.make (Dag.size sim.dag) [] in
  let () =
    Simulator.history sim
    |> Seq.iter (fun vtx ->
           confirmed.(Dag.id vtx) <- true;
           rewards.(Dag.id vtx) <- Ref.reward vtx)
  in
  let path =
    let open Fpath in
    v "." / "data" / "viz" // fpath (Task t)
  in
  let open Bos.OS in
  let d = Dir.create ~path:true (Fpath.parent path) in
  Result.bind d (fun _ ->
      File.with_oc path print_dag (sim, confirmed, rewards, legend (Task t), Ref.info))
  |> Result.join
  |> Rresult.R.failwith_error_msg
;;

let () =
  Random.self_init ();
  List.iter run tasks
;;
