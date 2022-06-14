open Intf
open Cpr_lib.Next

(* Gym parameters *)
module Parameters : sig
  type t = private
    { alpha : float (** attacker compute; 0 <= x <= 1 *)
    ; gamma : float (** attacker connection; 0 <= x <= 1 *)
    ; defenders : int (** number of defenders; 1 <= x *)
    ; activation_delay : float (** difficulty; 0 < x *)
    ; max_steps : int (** termination criterion, number of attacker steps; 1 <= x *)
    ; max_time : float (** termination criterion, simulated time; 0 < x *)
    }

  (** raises Failure for invalid parameter combinations *)
  val t
    :  alpha:float
    -> gamma:float
    -> defenders:int
    -> activation_delay:float
    -> max_steps:int
    -> max_time:float
    -> t
end = struct
  type t =
    { alpha : float
    ; gamma : float
    ; defenders : int
    ; activation_delay : float
    ; max_steps : int
    ; max_time : float
    }

  let t ~alpha ~gamma ~defenders ~activation_delay ~max_steps ~max_time =
    let () =
      if alpha < 0. || alpha > 1. then failwith "alpha < 0 || alpha > 1";
      if gamma < 0. || gamma > 1. then failwith "gamma < 0 || gamma > 1";
      if defenders < 1 then failwith "defenders < 0";
      if activation_delay <= 0. then failwith "activation_delay <= 0";
      if max_steps <= 0 then failwith "max_steps <= 0";
      if max_time <= 0. then failwith "max_time <= 0"
    in
    { alpha; gamma; defenders; activation_delay; max_steps; max_time }
  ;;
end

module type AttackSpace = sig
  type data

  val info : string

  module Protocol : Protocol with type data = data

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

  module Agent (V : LocalView with type data = data) : sig
    open V

    type state
    type observable_state

    val preferred : state -> env Dag.vertex
    val puzzle_payload : state -> (env, data) puzzle_payload
    val init : roots:env Dag.vertex list -> state
    val prepare : state -> env event -> observable_state
    val observe : observable_state -> Observation.t
    val apply : observable_state -> Action.t -> (env, state) handler_return
  end

  val policies : (Observation.t -> Action.t) Collection.t
end

(* This is just for checking whether the above might actually work *)
module _ : AttackSpace = struct
  module Protocol = Cpr_protocols.Nakamoto
  include Protocol.SszAttack
end

type 'data agent =
  | Agent :
      { preferred : 'state -> 'data Simulator.env Dag.vertex
      ; puzzle_payload : 'state -> ('data Simulator.env, 'data) puzzle_payload
      ; init : roots:'data Simulator.env Dag.vertex list -> 'state
      ; prepare : 'state -> 'data Simulator.env event -> 'observable
      ; observe : 'observable -> floatarray
      ; observe_hum : 'observable -> string
      ; apply : 'observable -> int -> ('data Simulator.env, 'state) handler_return
      ; mutable state : 'observable
      }
      -> 'data agent

(* state of a running (but paused) simulation *)
type 'data instance =
  { sim : 'data Simulator.state
  ; agent : 'data agent
  ; mutable reward_applied_upto : 'data Simulator.env Dag.vertex
  ; mutable last_time : float
  ; mutable steps : int
  }

let numeration ?(sep = ", ") ?(conj = "and") =
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
    (type data)
    (module M : AttackSpace with type data = data)
    ~(reward : string)
    (p : Parameters.t)
    : data instance ref env
  =
  let reward_function =
    let collection = M.Protocol.reward_functions () in
    match Collection.get reward collection with
    | Some x -> x.it
    | None ->
      let msg =
        "unkown reward function '"
        ^ reward
        ^ "'. Try "
        ^ (Collection.keys collection
          |> List.map (fun s -> "'" ^ s ^ "'")
          |> numeration ~conj:"or")
        ^ "."
      in
      failwith msg
  in
  let network =
    Network.T.selfish_mining
      ~activation_delay:p.activation_delay
      ~gamma:p.gamma
      ~alpha:p.alpha
      ~defenders:p.defenders
      ~propagation_delay:0.00001
  in
  let rec skip_to_interaction sim =
    let open Simulator in
    match dequeue sim with
    | Some (ForNode (0, ev)) -> ev
    | Some ev ->
      handle_event sim ev;
      skip_to_interaction sim
    | None -> failwith "simulation should continue forever"
  in
  let init () =
    let open Simulator in
    let patch = function
      | 0 -> Some (dummy_node (module M.Protocol))
      | _ -> None
    in
    let sim = init ~patch (module M.Protocol) network in
    let agent =
      let (Node node) = sim.nodes.(0) in
      let _ = node.view in
      let (module LocalView : LocalView
            with type env = data Simulator.env
             and type data = data)
        =
        node.view
      in
      let open M.Agent (LocalView) in
      let observe s = observe s |> M.Observation.to_floatarray
      and observe_hum s = observe s |> M.Observation.to_string
      and apply s i = apply s (M.Action.of_int i)
      and state = skip_to_interaction sim |> prepare (init ~roots:sim.roots) in
      Agent
        { preferred; init; puzzle_payload; observe; observe_hum; apply; prepare; state }
    in
    { sim; agent; reward_applied_upto = List.hd sim.roots; last_time = 0.; steps = 0 }
  in
  let observe t =
    let (Agent a) = t.agent in
    a.observe a.state
  and observe_hum t =
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
    let t = !ref_t in
    let () = t.steps <- t.steps + 1 in
    let (Agent a) = t.agent in
    (* Apply action i to the simulator state. *)
    let state =
      let ret = a.apply a.state action in
      let () =
        let open Simulator in
        List.iter (fun m -> schedule t.sim.clock 0. (FromNode (0, Share m))) ret.share
      in
      ret.state
    in
    (* Fast forward simulation till next attacker action. *)
    let event = skip_to_interaction t.sim in
    let () = a.state <- a.prepare state event in
    (* End simulation? *)
    let done_, apply_upto =
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
      if t.steps < p.max_steps && t.sim.clock.now < p.max_time
      then (
        let ca =
          List.to_seq prefs |> Dag.common_ancestor' t.sim.global_view |> Option.get
        in
        false, ca)
      else true, M.Protocol.judge t.sim.global_view_m prefs
    in
    (* Calculate rewards for the new common blocks. *)
    (* 1. find common ancestor *)
    (* 2. apply reward function up to last marked DAG vertex (exclusive) *)
    let cf (* cash flow *) = Array.make 2 0. in
    let reward_time_elapsed, reward_n_pows, simulator_clock_rewarded =
      let last_ca_time = (Dag.data t.reward_applied_upto).appended_at in
      if apply_upto $== t.reward_applied_upto
      then 0., 0, last_ca_time
      else (
        let pow_cnt = ref 0 in
        let rec iter seq =
          match seq () with
          | Seq.Nil -> ()
          | Cons (n, _seq) when n $== t.reward_applied_upto -> ()
          | Cons (n, seq) ->
            if Option.is_some (Dag.data n).pow_hash then incr pow_cnt else ();
            reward_function
              ~view:t.sim.global_view_m
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
        iter (Dag.iterate_ancestors t.sim.global_view [ apply_upto ]);
        ( (Dag.data apply_upto).appended_at -. last_ca_time
        , !pow_cnt
        , (Dag.data apply_upto).appended_at ))
    in
    assert (cf.(0) = 0. || reward_time_elapsed > 0.);
    (* 3. mark common ancestor DAG vertex *)
    t.reward_applied_upto <- apply_upto;
    (* Calculate simulated time. *)
    let step_time = t.sim.clock.now -. t.last_time in
    t.last_time <- t.sim.clock.now;
    (* Return *)
    ( (* observation *)
      observe t
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
  }
;;
