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
    ; max_height : int (** termination criterion, block height; 0 < x *)
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
    -> max_height:int
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
    ; max_height : int
    ; max_progress : float
    ; max_time : float
    }

  let t
      ~alpha
      ~gamma
      ~defenders
      ~activation_delay
      ~max_steps
      ~max_height
      ~max_progress
      ~max_time
    =
    let () =
      if alpha < 0. || alpha > 1. then failwith "alpha < 0 || alpha > 1";
      if gamma < 0. || gamma > 1. then failwith "gamma < 0 || gamma > 1";
      if defenders < 1 then failwith "defenders < 0";
      if activation_delay <= 0. then failwith "activation_delay <= 0";
      if max_steps <= 0 then failwith "max_steps <= 0";
      if max_height <= 0 then failwith "max_height <= 0";
      if max_progress <= 0. then failwith "max_progress <= 0";
      if max_time <= 0. then failwith "max_time <= 0"
    in
    { alpha
    ; gamma
    ; defenders
    ; activation_delay
    ; max_steps
    ; max_height
    ; max_progress
    ; max_time
    }
  ;;
end

type 'data agent =
  | Agent :
      { preferred : 'state -> 'data Simulator.env Dag.vertex
      ; puzzle_payload : 'state -> ('data Simulator.env, 'data) draft_vertex
      ; init : roots:'data Simulator.env Dag.vertex list -> 'state
      ; prepare : 'state -> 'data Simulator.env event -> 'observable
      ; observe : 'observable -> floatarray
      ; observe_hum : 'observable -> string
      ; apply : 'observable -> int -> ('data Simulator.env, 'data, 'state) action
      ; mutable state : 'observable
      }
      -> 'data agent

(* state of a running (but paused) simulation *)
type instance =
  | Instance :
      { sim : 'data Simulator.state
      ; agent : 'data agent
      ; reward_function : 'data Simulator.env reward_function
      ; protocol : (module Protocol with type data = 'data)
      ; mutable episode_steps : int
      ; mutable last_chain_time : float
      ; mutable last_sim_time : float
      ; mutable last_reward_attacker : float
      ; mutable last_reward_defender : float
      ; mutable last_n_pow : int
      }
      -> instance

let numeration ?(sep = ", ") ?(conj = " and ") =
  let rec f acc = function
    | [] -> assert false
    | [ hd ] -> acc ^ sep ^ conj ^ hd
    | hd :: tl -> f (acc ^ sep ^ hd) tl
  in
  function
  | [] -> ""
  | [ a ] -> a
  | [ a; b ] -> a ^ conj ^ b
  | x -> f "" x
;;

let dummy_node
    (type env data)
    (module P : Protocol with type data = data)
    (view : (env, data) local_view)
    : (env, data) node
  =
  let (Node (module Honest)) = P.honest view in
  Node
    (module struct
      include Honest

      let handler _state _event = failwith "dummy node handler must not be called"
    end)
;;

let of_module
    ?(logger = Log.dummy_logger)
    (AttackSpace (module M))
    ~(reward : string)
    (p : Parameters.t)
    : instance ref env
  =
  let network =
    Network.T.selfish_mining
      ~activation_delay:p.activation_delay
      ~gamma:p.gamma
      ~alpha:p.alpha
      ~defenders:p.defenders
      ~propagation_delay:0.00001
  in
  let rec skip_to_interaction sim puzzle_payload =
    let open Simulator in
    match dequeue sim with
    | Some (ForNode (0, ev)) -> ev
    | Some (FromNode (0, PowProposal _)) ->
      let payload = puzzle_payload () in
      let vertex =
        match append ~pow:true sim 0 payload with
        | `Global_fresh x ->
          let () = check_vertex sim x in
          x
        | `Local_fresh _ | `Redundant _ -> assert false
      in
      schedule sim.clock `Now (ForNode (0, ProofOfWork vertex));
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
    let reward_function =
      let (module Ref) = sim.referee in
      match Collection.get reward Ref.reward_functions with
      | Some x -> x.it
      | None ->
        let msg =
          "unkown reward function '"
          ^ reward
          ^ "'. Try "
          ^ (Collection.keys Ref.reward_functions
            |> List.map (fun s -> "'" ^ s ^ "'")
            |> numeration ~conj:" or ")
          ^ "."
        in
        failwith msg
    in
    let agent =
      let (Node node) = sim.nodes.(0) in
      let _ = node.view in
      let (module LocalView : LocalView with type env = _ and type data = _) =
        node.view
      in
      let open M.Agent (LocalView) in
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
      ; reward_function
      ; protocol = (module M.Protocol)
      ; episode_steps = 0
      ; last_chain_time = 0.
      ; last_sim_time = 0.
      ; last_reward_attacker = 0.
      ; last_reward_defender = 0.
      ; last_n_pow = 0
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
      let ret = a.apply a.state action in
      let () =
        let open Simulator in
        List.iter
          (fun m -> schedule t.sim.clock `Immediate (FromNode (0, Share m)))
          ret.share
      in
      ret.state
    in
    let () = t.episode_steps <- t.episode_steps + 1 in
    (* Fast forward simulation till next attacker action. *)
    let () =
      let event = skip_to_interaction t.sim (fun () -> a.puzzle_payload state) in
      a.state <- a.prepare state event
    in
    (* End simulation? *)
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
    let height = Protocol.height (Dag.data head).value
    and progress = Protocol.progress (Dag.data head).value in
    let done_ =
      not
        (t.episode_steps < p.max_steps
        && height < p.max_height
        && progress < p.max_progress
        && t.sim.clock.now < p.max_time)
    in
    (* TODO. We calculate rewards for the whole chain on each step, then return the delta.
       If this turns out to be to expensive, we can record safepoints and use them for
       caching. *)
    let reward_attacker = ref 0.
    and reward_defender = ref 0.
    and n_pow = ref 0 in
    let () =
      let f vertex =
        let open Simulator in
        if Option.is_some (Dag.data vertex).pow then incr n_pow else ();
        t.reward_function
          ~assign:(fun reward vertex ->
            match reward_recipient vertex with
            | None -> ()
            | Some 0 -> reward_attacker := !reward_attacker +. reward
            | Some _ -> reward_defender := !reward_defender +. reward)
          vertex
      in
      Ref.history head |> Seq.iter f
    in
    let chain_time = Simulator.timestamp (Dag.data head)
    and sim_time = t.sim.clock.now in
    (* Calculate return/info metrics *)
    let reward = !reward_attacker -. t.last_reward_attacker
    and info =
      let float = Py.Float.of_float
      and int = Py.Int.of_int in
      [ "step_reward_attacker", float (!reward_attacker -. t.last_reward_attacker)
      ; "step_reward_defender", float (!reward_defender -. t.last_reward_defender)
      ; "step_n_pow", int (!n_pow - t.last_n_pow)
      ; "step_chain_time", float (chain_time -. t.last_chain_time)
      ; "step_sim_time", float (sim_time -. t.last_sim_time)
      ; "episode_reward_attacker", float !reward_attacker
      ; "episode_reward_defender", float !reward_defender
      ; "episode_n_pow", int !n_pow
      ; "episode_chain_time", float chain_time
      ; "episode_sim_time", float sim_time
      ; "episode_n_steps", int t.episode_steps
      ; "episode_height", int height
      ; "episode_progress", float progress
      ]
      @
      if done_
      then
        [ "episode_pow_interval", float (chain_time /. float_of_int !n_pow)
        ; "episode_block_interval", float (chain_time /. float_of_int height)
        ]
      else []
    in
    (* update state *)
    t.last_chain_time <- chain_time;
    t.last_sim_time <- sim_time;
    t.last_reward_attacker <- !reward_attacker;
    t.last_reward_defender <- !reward_defender;
    t.last_n_pow <- !n_pow;
    (* return *)
    observe (Instance t), reward, done_, info
  and low = M.Observation.(low |> to_floatarray)
  and high = M.Observation.(high |> to_floatarray)
  and to_string t =
    Printf.sprintf
      "%s; %s; α=%.2f attacker\n%s\nActions: %s"
      M.Protocol.info
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
  ; low
  ; high
  ; to_string
  ; policies
  ; puzzles_per_block = M.Protocol.puzzles_per_block
  }
;;
