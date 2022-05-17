open Intf

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

open Cpr_lib

module type Observation = sig
  type t
  type data
  type state

  val length : int
  val low : t
  val high : t
  val observe : (data Simulator.data, data) local_view -> state -> t
  val to_floatarray : t -> floatarray
  val of_floatarray : floatarray -> t
  val to_string : t -> string
end

(* interface to simulated protocols *)
module type AttackSpace = sig
  type data
  type state

  val description : string

  val protocol
    : (data Simulator.data, data, Simulator.pow, data Simulator.data Dag.vertex) protocol

  val reward_function : (data Simulator.data, data) reward_function

  module Observation : Observation with type data := data and type state := state

  module Action : sig
    type t

    val n : int
    val to_string : t -> string
    val to_int : t -> int
    val of_int : int -> t
  end

  val policies : (string * (Observation.t -> Action.t)) list
  val node : (data Simulator.data, data, Simulator.pow, state) node

  val apply_action
    :  (data Simulator.data, data) local_view
    -> (data Simulator.data, data, Simulator.pow) actions
    -> state
    -> Action.t
    -> state

  val shutdown
    :  (data Simulator.data, data) local_view
    -> (data Simulator.data, data, Simulator.pow) actions
    -> state
    -> state
end

(* It's useful on the RL side to have the alpha parameter in the observation. This enables
   to train one policy for multiple alphas in one go. This function augments an
   observation space with a constant alpha. *)
let augment_observation
    ~alpha
    (type r s t)
    (module M : Observation with type t = t and type state = s and type data = r)
  =
  (module struct
    type data = M.data
    type state = M.state
    type t = M.t * float

    let length = M.length + 1
    let observe x y = M.observe x y, alpha
    let low = M.low, 0.
    let high = M.high, 1.

    let to_floatarray (t, alpha) =
      let a = Float.Array.make 1 alpha in
      Float.Array.append (M.to_floatarray t) a
    ;;

    let of_floatarray a =
      let alpha = Float.Array.get a (length - 1)
      and arr = Float.Array.make M.length Float.nan in
      let () = Float.Array.blit a 0 arr 0 M.length in
      M.of_floatarray arr, alpha
    ;;

    let to_string (t, _alpha) = M.to_string t
  end : Observation
    with type t = t * float
     and type state = s
     and type data = r)
;;

(* state of a running simulation *)
type ('a, 'b) instance =
  { sim : 'a Simulator.state
  ; attacker : 'b
  ; mutable reward_applied_upto : 'a Simulator.data Dag.vertex option
  ; mutable last_time : float
  ; mutable steps : int
  }

let of_module
    (type r s)
    (module M : AttackSpace with type state = s and type data = r)
    (p : Parameters.t)
    : ( r
      , (r Simulator.data, r) local_view
        * (r Simulator.data, r, Simulator.pow) actions
        * (r, s) Simulator.node' )
      instance
      ref
    env
  =
  let (module O) =
    augment_observation
      ~alpha:p.alpha
      (module struct
        include M.Observation

        type state = s
        type data = r
      end)
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
    let v, a, n = patch ~node:0 M.node setup in
    { sim = Simulator.init setup
    ; attacker = v, a, n
    ; reward_applied_upto = None
    ; last_time = 0.
    ; steps = 0
    }
  in
  let rec skip_to_interaction sim =
    let open Simulator in
    match dequeue sim with
    | Some ev ->
      handle_event ~activations:(-1) sim ev;
      if ev.node = 0 then () else skip_to_interaction sim
    | None -> failwith "simulation should continue forever"
  and observe t =
    let v, _a, (n : _ Simulator.node') = t.attacker in
    O.observe v n.state
  in
  let create () =
    let t = init () in
    skip_to_interaction t.sim;
    ref t
  and reset ref_t =
    let t = init () in
    skip_to_interaction t.sim;
    ref_t := t;
    observe t |> O.to_floatarray
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
    let v, (a : _ actions), (n : _ Simulator.node') = t.attacker in
    let action = M.Action.of_int i in
    n.state <- M.apply_action v a n.state action;
    (* Continue simulation till next attacker action. *)
    skip_to_interaction t.sim;
    (* End simulation? *)
    let done_ =
      if t.steps < p.max_steps && t.sim.clock.now < p.max_time
      then false
      else (
        n.state <- M.shutdown v a n.state;
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
            M.reward_function
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
      observe t |> O.to_floatarray
    , (* reward *)
      cf.(0) *. reward_time_elapsed
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
  and low = O.(low |> to_floatarray)
  and high = O.(high |> to_floatarray)
  and to_string t =
    Printf.sprintf
      "Protocol %s against α=%.2f attacker\n%s\nActions: %s"
      M.description
      p.alpha
      (observe !t |> O.to_string)
      actions_hum
  and policies =
    List.map
      (fun (name, p) -> name, fun a -> O.of_floatarray a |> fst |> p |> M.Action.to_int)
      M.policies
  in
  { n_actions = M.Action.n
  ; observation_length = O.length
  ; create
  ; reset
  ; step
  ; low
  ; high
  ; to_string
  ; policies
  }
;;
