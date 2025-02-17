open Intf
open Cpr_lib

(* Gym parameters *)
module Parameters : sig
  type t = private
    { alpha : float (** attacker compute; 0 <= x <= 1 *)
    ; gamma : float (** attacker connection; 0 <= x <= 1 *)
    ; defenders : int (** number of defenders; 1 <= x *)
    ; activation_delay : float (** difficulty; 0 < x *)
    ; max_steps : int (** termination criterion, number of attacker steps; 1 <= x *)
    ; max_progress : float (** termination criterion, block float; 0 < x *)
    ; max_time : float (** termination criterion, simulated time; 0 < x *)
    }

  (** raises Failure for invalid parameter combinations *)
  val t
    :  alpha:float
    -> gamma:float
    -> defenders:int
    -> activation_delay:float
    -> max_steps:int
    -> max_progress:float
    -> max_time:float
    -> t
end = struct
  type t =
    { alpha : float
    ; gamma : float
    ; defenders : int
    ; activation_delay : float
    ; max_steps : int
    ; max_progress : float
    ; max_time : float
    }

  let t ~alpha ~gamma ~defenders ~activation_delay ~max_steps ~max_progress ~max_time =
    let () =
      if Float.is_nan activation_delay then failwith "activation_delay cannot be NaN";
      if Float.is_nan alpha then failwith "alpha cannot be NaN";
      if Float.is_nan gamma then failwith "gamma cannot be NaN";
      if alpha < 0. || alpha > 1. then failwith "alpha < 0 || alpha > 1";
      if gamma < 0. || gamma > 1. then failwith "gamma < 0 || gamma > 1";
      if defenders < 1 then failwith "defenders < 0";
      if activation_delay <= 0. then failwith "activation_delay <= 0";
      if max_steps <= 0 then failwith "max_steps <= 0";
      if max_progress <= 0. then failwith "max_progress <= 0";
      if max_time <= 0. then failwith "max_time <= 0"
    in
    { alpha; gamma; defenders; activation_delay; max_steps; max_progress; max_time }
  ;;
end

type 'data agent =
  | Agent :
      { preferred : 'state -> 'data Simulator.block
      ; puzzle_payload : 'state -> ('data Simulator.block, 'data) block_draft
      ; init : roots:'data Simulator.block list -> 'state
      ; prepare : 'state -> 'data Simulator.block event -> 'observable
      ; observe : 'observable -> floatarray
      ; observe_hum : 'observable -> string
      ; apply : 'observable -> int -> ('data Simulator.block, 'data, 'state) action
      ; mutable state : 'observable
      }
      -> 'data agent

(* state of a running (but paused) simulation *)
type instance =
  | Instance :
      { sim : 'data Simulator.state
      ; agent : 'data agent
      ; protocol : (module Protocol with type data = 'data)
      ; mutable episode_steps : int
      ; mutable last_progress : float
      ; mutable last_chain_time : float
      ; mutable last_sim_time : float
      ; mutable last_reward_attacker : float
      ; mutable last_reward_defender : float
      }
      -> instance

let dummy_node
    (type block data)
    (module P : Protocol with type data = data)
    (view : (block, data) view)
    : (block, data) node
  =
  let (Node (module Honest)) = P.honest view in
  Node
    (module struct
      include Honest

      let handler _state _event = failwith "dummy node handler must not be called"
    end)
;;

let of_module ?(logger = Log.dummy_logger) (AttackSpace (module M)) (p : Parameters.t)
    : instance ref env
  =
  let network =
    Network.T.selfish_mining
      ~activation_delay:p.activation_delay
      ~gamma:p.gamma
      ~alpha:p.alpha
      ~defenders:p.defenders
      ~propagation_delay:1e-9
  in
  let rec skip_to_interaction sim puzzle_payload =
    let open Simulator in
    match dequeue sim with
    | Some (OnNode (0, ev)) -> ev
    | Some (Dag (0, `ProofOfWork, _draft)) ->
      let draft = puzzle_payload () in
      let vertex = append ~pow:true sim 0 draft in
      schedule sim.clock `Now (MakeVisible (0, `ProofOfWork, vertex));
      skip_to_interaction sim puzzle_payload
    | Some ev ->
      handle_event sim ev;
      skip_to_interaction sim puzzle_payload
    | None -> failwith "simulation should continue forever"
  in
  let init () =
    let open Simulator in
    let patch = function
      | 0 -> Some (dummy_node (module M.Protocol))
      | _ -> None
    in
    let sim = init ~logger ~patch (module M.Protocol) network in
    let agent =
      let (Node node) = sim.nodes.(0) in
      let _ = node.view in
      let (module View : View with type block = _ and type data = _) = node.view in
      let open M.Agent (View) in
      let observe s = observe s |> M.Observation.to_floatarray
      and observe_hum s = observe s |> M.Observation.to_string
      and apply s i = apply s (M.Action.of_int i)
      and state =
        let init = init ~roots:sim.roots in
        let puzzle_payload () = puzzle_payload init in
        skip_to_interaction sim puzzle_payload |> prepare init
      in
      Agent
        { preferred; init; puzzle_payload; observe; observe_hum; apply; prepare; state }
    in
    Instance
      { sim
      ; agent
      ; protocol = (module M.Protocol)
      ; episode_steps = 0
      ; last_progress = 0.
      ; last_chain_time = 0.
      ; last_sim_time = 0.
      ; last_reward_attacker = 0.
      ; last_reward_defender = 0.
      }
  in
  let observe (Instance t) =
    let (Agent a) = t.agent in
    a.observe a.state
  and observe_hum (Instance t) =
    let (Agent a) = t.agent in
    a.observe_hum a.state
  in
  let create () =
    let t = init () in
    ref t
  and reset ref_t =
    let t = init () in
    ref_t := t;
    observe t
  and actions_hum =
    let open M.Action in
    List.init n (fun i -> Printf.sprintf "(%d) %s" i (of_int i |> to_string))
    |> String.concat " | "
  in
  let step ref_t ~action =
    (* env.step () *)
    let (Instance t) = !ref_t in
    let (module Ref) = t.sim.referee in
    let (Agent a) = t.agent in
    let (module Protocol) = t.protocol in
    (* Apply action i to the simulator state. *)
    let state =
      let act = a.apply a.state action in
      Simulator.handle_action t.sim 0 act;
      act.state
    in
    let () = t.episode_steps <- t.episode_steps + 1 in
    (* Fast forward simulation till next attacker action. *)
    let () =
      let event = skip_to_interaction t.sim (fun () -> a.puzzle_payload state) in
      a.state <- a.prepare state event
    in
    (* Find objectively best tip *)
    let prefs =
      Array.mapi
        Simulator.(
          function
          | 0 -> fun _ -> a.preferred state
          | _ ->
            (function
            | Node n -> n.preferred n.state))
        t.sim.nodes
      |> Array.to_list
    in
    let head = Ref.winner prefs in
    (* End simulation? *)
    let progress = Ref.progress head in
    let done_ =
      not
        (t.episode_steps < p.max_steps
        && progress < p.max_progress
        && t.sim.clock.now < p.max_time)
    in
    let reward_attacker, reward_defender, _ =
      Float.Array.fold_left
        (fun (a, d, i) x -> if i = 0 then a +. x, d, i + 1 else a, d +. x, i + 1)
        (0., 0., 0)
        (Dag.data head).rewards
    and chain_time = Simulator.timestamp (Dag.data head)
    and sim_time = t.sim.clock.now in
    (* Calculate return/info metrics *)
    let reward = reward_attacker -. t.last_reward_attacker
    and info =
      let open Info in
      [ float "step_reward_attacker" (reward_attacker -. t.last_reward_attacker)
      ; float "step_reward_defender" (reward_defender -. t.last_reward_defender)
      ; float "step_progress" (progress -. t.last_progress)
      ; float "step_chain_time" (chain_time -. t.last_chain_time)
      ; float "step_sim_time" (sim_time -. t.last_sim_time)
      ; float "episode_reward_attacker" reward_attacker
      ; float "episode_reward_defender" reward_defender
      ; float "episode_progress" progress
      ; float "episode_chain_time" chain_time
      ; float "episode_sim_time" sim_time
      ; int "episode_n_steps" t.episode_steps
      ; int "episode_n_activations" t.sim.clock.c_activations
      ]
      @ Info.prefix_key "protocol_" Protocol.info
      @ Info.prefix_key "head_" (Ref.info head)
    in
    (* update state *)
    t.last_chain_time <- chain_time;
    t.last_sim_time <- sim_time;
    t.last_reward_attacker <- reward_attacker;
    t.last_reward_defender <- reward_defender;
    t.last_progress <- progress;
    (* return *)
    observe (Instance t), reward, done_, info
  and to_string t =
    Printf.sprintf
      "%s; %s; α=%.2f attacker\n%s\nActions: %s"
      M.Protocol.description
      M.info
      p.alpha
      (observe_hum !t)
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
  ; low = M.Observation.low
  ; high = M.Observation.high
  ; to_string
  ; policies
  }
;;
