open Intf
open Cpr_lib
module Parameters = Engine.Parameters

module type AttackSpace = sig
  type env
  type data
  type pow
  type agent_state
  type honest_state

  val description : string
  val protocol : (env, data, pow, honest_state) protocol

  module Observation : sig
    type t

    val length : int
    val low : t
    val high : t
    val to_floatarray : t -> floatarray
    val of_floatarray : floatarray -> t
    val to_string : t -> string
  end

  module Action : sig
    type t

    val n : int
    val to_string : t -> string
    val to_int : t -> int
    val of_int : int -> t
  end

  val noop_node : (env, data, pow, agent_state) node

  type pre_action

  val prepare
    :  (env, data) local_view
    -> (env, data, pow) actions
    -> agent_state
    -> (env, pow) event
    -> pre_action

  val observe : (env, data) local_view -> pre_action -> Observation.t

  val apply
    :  (env, data) local_view
    -> (env, data, pow) actions
    -> pre_action
    -> Action.t
    -> agent_state

  val shutdown
    :  (env, data) local_view
    -> (env, data, pow) actions
    -> agent_state
    -> agent_state

  val policies : (Observation.t -> Action.t) Collection.t
end

(* state of a running simulation *)
type ('data, 'honest_state, 'agent_state, 'pre_action) instance =
  { sim : 'data Simulator.state
  ; attacker_view : ('data Simulator.data, 'data) local_view
  ; attacker_actions : ('data Simulator.data, 'data, Simulator.pow) actions
  ; attacker_node : ('data, 'agent_state) Simulator.node'
  ; mutable current_event : ('data Simulator.data, Simulator.pow) event option
  ; mutable current_state : 'pre_action option
  ; mutable reward_applied_upto : 'data Simulator.data Dag.vertex option
  ; mutable last_time : float
  ; mutable steps : int
  }

let of_module
    (type data honest_state agent_state pre_action)
    (module M : AttackSpace
      with type env = data Simulator.data
       and type data = data
       and type pow = Simulator.pow
       and type honest_state = honest_state
       and type agent_state = agent_state
       and type pre_action = pre_action)
    (p : Parameters.t)
    (reward : string)
    : (data, honest_state, agent_state, pre_action) instance ref env
  =
  let reward_function =
    match Collection.get reward M.protocol.reward_functions with
    | Some x -> x.it
    | None -> failwith "invalid reward function"
  in
  let network =
    Network.T.selfish_mining
      ~activation_delay:p.activation_delay
      ~gamma:p.gamma
      ~alpha:p.alpha
      ~defenders:p.defenders
      ~propagation_delay:0.00001
  in
  let init () =
    let open Simulator in
    let setup = all_honest network M.protocol in
    let attacker_view, attacker_actions, attacker_node =
      patch ~node:0 M.noop_node setup
    in
    { sim = Simulator.init setup
    ; attacker_view
    ; attacker_actions
    ; attacker_node
    ; current_event = None
    ; current_state = None
    ; reward_applied_upto = None
    ; last_time = 0.
    ; steps = 0
    }
  in
  let rec skip_to_interaction t =
    let open Simulator in
    match dequeue t.sim with
    | Some ev ->
      handle_event ~activations:(-1) t.sim ev;
      if ev.node = 0
      then (
        t.current_event <- Some ev.event;
        t.current_state
          <- Some
               (M.prepare
                  t.attacker_view
                  t.attacker_actions
                  t.attacker_node.state
                  ev.event))
      else skip_to_interaction t
    | None -> failwith "simulation should continue forever"
  and observe t = M.observe t.attacker_view (t.current_state |> Option.get) in
  let create () =
    let t = init () in
    skip_to_interaction t;
    ref t
  and reset ref_t =
    let t = init () in
    skip_to_interaction t;
    ref_t := t;
    observe t |> M.Observation.to_floatarray
  and actions_hum =
    let open M.Action in
    List.init n (fun i -> Printf.sprintf "(%d) %s" i (of_int i |> to_string))
    |> String.concat " | "
  in
  let step ref_t ~action:i =
    (* env.step () *)
    let t = !ref_t in
    let () = t.steps <- t.steps + 1 in
    (* Apply action i to the simulator state. *)
    let action = M.Action.of_int i in
    t.attacker_node.state
      <- M.apply t.attacker_view t.attacker_actions (t.current_state |> Option.get) action;
    t.current_state <- None;
    t.current_event <- None;
    (* Fast forward simulation till next attacker action. *)
    skip_to_interaction t;
    (* End simulation? *)
    let done_ =
      if t.steps < p.max_steps && t.sim.clock.now < p.max_time
      then false
      else (
        t.attacker_node.state
          <- M.shutdown t.attacker_view t.attacker_actions t.attacker_node.state;
        true)
    in
    (* Calculate rewards for the new common blocks. *)
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
    (* 2. apply reward function up to last marked DAG vertex (exclusive) *)
    let ( $=? ) n m = Option.map (fun m -> m $== n) m |> Option.value ~default:false in
    let cf (* cash flow *) = Array.make 2 0. in
    let reward_time_elapsed, reward_n_pows, simulator_clock_rewarded =
      let last_ca_time =
        Option.map Simulator.(fun x -> (Dag.data x).appended_at) t.reward_applied_upto
        |> Option.value ~default:0.
      in
      if ca $=? t.reward_applied_upto
      then 0., 0, last_ca_time
      else (
        let pow_cnt = ref 0 in
        let rec iter seq =
          match seq () with
          | Seq.Nil -> ()
          | Cons (n, _seq) when n $=? t.reward_applied_upto -> ()
          | Cons (n, seq) ->
            if Option.is_some (Dag.data n).pow_hash then incr pow_cnt else ();
            reward_function
              ~view:t.sim.global
              ~assign:(fun x n ->
                match (Dag.data n).appended_by with
                | None -> ()
                | Some i ->
                  (* accumulate all defender rewards in cf.(1) *)
                  let i = min i 1 in
                  cf.(i) <- cf.(i) +. x)
              n;
            iter seq
        in
        iter (Dag.iterate_ancestors t.sim.global.view [ ca ]);
        (Dag.data ca).appended_at -. last_ca_time, !pow_cnt, (Dag.data ca).appended_at)
    in
    assert (cf.(0) = 0. || reward_time_elapsed > 0.);
    (* 3. mark common ancestor DAG vertex *)
    t.reward_applied_upto <- Some ca;
    (* Calculate simulated time. *)
    let step_time = t.sim.clock.now -. t.last_time in
    t.last_time <- t.sim.clock.now;
    (* Return *)
    ( (* observation *)
      observe t |> M.Observation.to_floatarray
    , (* reward *)
      cf.(0)
    , done_
    , (* info dict *)
      [ "reward_attacker", Py.Float.of_float cf.(0)
      ; "reward_defender", Py.Float.of_float cf.(1)
      ; "reward_time_elapsed", Py.Float.of_float reward_time_elapsed
      ; "reward_n_pows", Py.Int.of_int reward_n_pows
      ; "step_time_elapsed", Py.Float.of_float step_time
      ; "simulator_clock_now", Py.Float.of_float t.sim.clock.now
      ; "simulator_clock_rewarded", Py.Float.of_float simulator_clock_rewarded
      ] )
  and low = M.Observation.(low |> to_floatarray)
  and high = M.Observation.(high |> to_floatarray)
  and to_string t =
    Printf.sprintf
      "Protocol %s against α=%.2f attacker\n%s\nActions: %s"
      M.description
      p.alpha
      (observe !t |> M.Observation.to_string)
      actions_hum
  and policies =
    Collection.map_to_list
      (fun e -> e.key, fun a -> M.Observation.of_floatarray a |> e.it |> M.Action.to_int)
      M.policies
  in
  { n_actions = M.Action.n
  ; observation_length = M.Observation.length
  ; create
  ; reset
  ; step
  ; low
  ; high
  ; to_string
  ; policies
  }
;;
