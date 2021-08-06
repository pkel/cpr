open Cpr_lib
open Cpr_protocols
open Intf

type ('a, 'b) instance =
  { sim : 'a Simulator.state
  ; attacker : 'b
  ; mutable reward_applied_to : 'a Simulator.data Dag.node option
  ; mutable last_time : float
  }

let bk ~alpha ~k ~(reward : _ Protocol.reward_function) : _ env =
  let actions = Array.of_list B_k.PrivateAttack.Action.all in
  let params : Simulator.params =
    let delay = Distributions.constant 0. in
    { activations = -1
    ; network =
        Network.
          { dissemination = Simple
          ; nodes =
              [| { compute = alpha; links = [ { dest = 1; delay } ] }
               ; { compute = 1. -. alpha; links = [ { dest = 0; delay } ] }
              |]
          }
    ; activation_delay = 1.
    }
  in
  let init () =
    let open Simulator in
    let protocol = B_k.protocol ~k in
    let setup = all_honest params protocol in
    let v, a, n = patch ~node:0 (PrivateAttack.withhold protocol.honest) setup in
    { sim = Simulator.init setup
    ; attacker = v, a, n
    ; reward_applied_to = None
    ; last_time = 0.
    }
  in
  let rec skip_to_interaction sim =
    let open Simulator in
    match dequeue sim with
    | Some ev ->
      handle_event params sim ev;
      if ev.node = 0 then () else skip_to_interaction sim
    | None -> failwith "simulation should continue forever"
  and observe t =
    let v, _a, (n : _ Simulator.node') = t.attacker in
    B_k.PrivateAttack.Observation.observe v n.state
  in
  let create () =
    let t = init () in
    skip_to_interaction t.sim;
    ref t
  and reset ref_t =
    let t = init () in
    skip_to_interaction t.sim;
    ref_t := t;
    observe t
  and actions_hum =
    let open B_k.PrivateAttack in
    List.mapi
      (fun i a -> Printf.sprintf "(%d) %s" i (Action.Variants.to_name a))
      Action.all
    |> String.concat " | "
  in
  let step ref_t ~action:i =
    let t = !ref_t in
    let v, a, (n : _ Simulator.node') = t.attacker in
    (* apply action *)
    let policy _ _ = actions.(i) in
    let tactic = B_k.PrivateAttack.tactic_of_policy ~k policy in
    n.state <- PrivateAttack.apply_tactic tactic v a n.state;
    (* continue simulation *)
    skip_to_interaction t.sim;
    (* reward *)
    (* 1. find common ancestor *)
    let ca =
      (* TODO move this into Simulator.common_ancestor *)
      Array.to_seq t.sim.nodes
      |> Seq.map
           Simulator.(
             function
             | Node n -> n.preferred n.state)
      |> Dag.common_ancestor' t.sim.global.view
      |> Option.get
    in
    (* 2. apply reward function up to last marked DAG node (exclusive) *)
    let ( $=? ) n m = Option.map (fun m -> m $== n) m |> Option.value ~default:false in
    let cf (* cash flow *) = Array.make 2 0. in
    let reward_time =
      if ca $=? t.reward_applied_to
      then 0.
      else (
        let rec iter seq =
          match seq () with
          | Seq.Nil -> ()
          | Cons (n, _seq) when n $=? t.reward_applied_to -> ()
          | Cons (n, seq) ->
            reward
              ~view:t.sim.global
              ~assign:(fun x n ->
                match (Dag.data n).appended_by with
                | None -> ()
                | Some i -> cf.(i) <- cf.(i) +. x)
              n;
            iter seq
        in
        iter (Dag.iterate_ancestors t.sim.global.view [ ca ]);
        let last_ca_time =
          Option.map Simulator.(fun x -> (Dag.data x).appended_at) t.reward_applied_to
          |> Option.value ~default:0.
        in
        (Dag.data ca).appended_at -. last_ca_time)
    in
    (* 3. mark common ancestor DAG node *)
    t.reward_applied_to <- Some ca;
    (* end reward *)
    let step_time = t.sim.clock.now -. t.last_time in
    t.last_time <- t.sim.clock.now;
    (* return *)
    ( observe t
    , cf.(0)
    , false
    , [ "attacker_reward", Py.Float.of_float cf.(0)
      ; "defender_reward", Py.Float.of_float cf.(1)
      ; "timedelta_reward", Py.Float.of_float reward_time
      ; "timedelta_step", Py.Float.of_float step_time
      ; "time", Py.Float.of_float t.sim.clock.now
      ] )
  and low = Float.Array.make B_k.PrivateAttack.Observation.n (-100.)
  (* TODO read actual numbers from Attack module *)
  and high = Float.Array.make B_k.PrivateAttack.Observation.n 100.
  and to_string t =
    Printf.sprintf
      "Bₖ with k=%d and α=%.2f\n%s\nActions: %s"
      k
      alpha
      (B_k.PrivateAttack.Observation.to_string_hum (observe !t))
      actions_hum
  and policies = B_k.PrivateAttack.policies in
  { n_actions = Array.length actions
  ; observation_length = B_k.PrivateAttack.Observation.n
  ; create
  ; reset
  ; step
  ; low
  ; high
  ; to_string
  ; policies
  }
;;

let default = bk ~k:51 ~alpha:0.25 ~reward:(B_k.constant_pow 1.)
