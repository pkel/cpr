open Intf
open Cpr_lib
open Cpr_protocols

type ('a, 'b) instance =
  { sim : 'a Simulator.state
  ; attacker : 'b
  ; mutable reward_applied_upto : 'a Simulator.data Dag.vertex option
  ; mutable last_time : float
  }

module type M = sig
  type data
  type state

  val description : string

  val protocol
    : (data Simulator.data, data, Simulator.pow, data Simulator.data Dag.vertex) protocol

  val reward_function : (data Simulator.data, data) reward_function

  module Observation : sig
    type t

    val length : int
    val low : t
    val high : t
    val observe : (data Simulator.data, data) local_view -> state -> t
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

  val policies : (string * (Observation.t -> Action.t)) list
  val node : (data Simulator.data, data, Simulator.pow, state) node

  val apply_action
    :  (data Simulator.data, data) local_view
    -> (data Simulator.data, data, Simulator.pow) actions
    -> state
    -> Action.t
    -> state
end

let of_module ~alpha (type s t) (module M : M with type state = s and type data = t)
    : ( t
      , (t Simulator.data, t) local_view
        * (t Simulator.data, t, Simulator.pow) actions
        * (t, s) Simulator.node' )
      instance
      ref
    env
  =
  let network : Network.t =
    let delay = Distributions.constant 0. in
    { dissemination = Simple
    ; nodes =
        [| { compute = alpha; links = [ { dest = 1; delay } ] }
         ; { compute = 1. -. alpha; links = [ { dest = 0; delay } ] }
        |]
    ; activation_delay = 1.
    }
  in
  let init () =
    let open Simulator in
    let setup = all_honest network M.protocol in
    let v, a, n = patch ~node:0 M.node setup in
    { sim = Simulator.init setup
    ; attacker = v, a, n
    ; reward_applied_upto = None
    ; last_time = 0.
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
    let open M.Observation in
    observe v n.state
  in
  let create () =
    let t = init () in
    skip_to_interaction t.sim;
    ref t
  and reset ref_t =
    let t = init () in
    skip_to_interaction t.sim;
    ref_t := t;
    observe t |> M.Observation.to_floatarray
  and actions_hum =
    let open M.Action in
    List.init n (fun i -> Printf.sprintf "(%d) %s" i (of_int i |> to_string))
    |> String.concat " | "
  in
  let step ref_t ~action:i =
    let t = !ref_t in
    let v, (a : _ actions), (n : _ Simulator.node') = t.attacker in
    (* apply action *)
    let action = M.Action.of_int i in
    n.state <- M.apply_action v a n.state action;
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
    (* 2. apply reward function up to last marked DAG vertex (exclusive) *)
    let ( $=? ) n m = Option.map (fun m -> m $== n) m |> Option.value ~default:false in
    let cf (* cash flow *) = Array.make 2 0. in
    let reward_time =
      if ca $=? t.reward_applied_upto
      then 0.
      else (
        let rec iter seq =
          match seq () with
          | Seq.Nil -> ()
          | Cons (n, _seq) when n $=? t.reward_applied_upto -> ()
          | Cons (n, seq) ->
            M.reward_function
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
          Option.map Simulator.(fun x -> (Dag.data x).appended_at) t.reward_applied_upto
          |> Option.value ~default:0.
        in
        (Dag.data ca).appended_at -. last_ca_time)
    in
    assert (cf.(0) = 0. || reward_time > 0.);
    (* 3. mark common ancestor DAG vertex *)
    t.reward_applied_upto <- Some ca;
    (* end reward *)
    let step_time = t.sim.clock.now -. t.last_time in
    t.last_time <- t.sim.clock.now;
    (* return *)
    ( observe t |> M.Observation.to_floatarray
    , (if reward_time > 0. then cf.(0) /. reward_time else 0.)
    , false
    , [ "attacker_reward", Py.Float.of_float cf.(0)
      ; "defender_reward", Py.Float.of_float cf.(1)
      ; "timedelta_reward", Py.Float.of_float reward_time
      ; "timedelta_step", Py.Float.of_float step_time
      ; "time", Py.Float.of_float t.sim.clock.now
      ] )
  and low = M.Observation.(low |> to_floatarray)
  and high = M.Observation.(high |> to_floatarray)
  and to_string t =
    Printf.sprintf
      "Protocol %s against α=%.2f attacker\n%s\nActions: %s"
      M.description
      alpha
      (observe !t |> M.Observation.to_string)
      actions_hum
  and policies =
    List.map
      (fun (name, p) ->
        name, fun a -> M.Observation.of_floatarray a |> p |> M.Action.to_int)
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

let nakamoto ~alpha ~reward =
  of_module
    ~alpha
    (module struct
      type data = Nakamoto.dag_data
      type state = data Simulator.data PrivateAttack.state

      let description = Printf.sprintf "Nakamoto"
      let protocol = Nakamoto.protocol
      let reward_function = reward

      include Nakamoto.PrivateAttack

      let node = PrivateAttack.withhold protocol.honest

      let apply_action v a state action =
        let tactic = tactic_of_policy (fun _ -> action) in
        PrivateAttack.apply_tactic tactic v a state
      ;;
    end)
;;

let bk ~alpha ~k ~reward =
  of_module
    ~alpha
    (module struct
      type data = B_k.dag_data
      type state = data Simulator.data PrivateAttack.state

      let description = Printf.sprintf "Bₖ with k=%d" k
      let protocol = B_k.protocol ~k
      let reward_function = reward

      include B_k.PrivateAttack

      let node = PrivateAttack.withhold protocol.honest

      let apply_action v a state action =
        let tactic = tactic_of_policy ~k (fun _ -> action) in
        PrivateAttack.apply_tactic tactic v a state
      ;;
    end)
;;

let bk_ll ~alpha ~k ~reward =
  of_module
    ~alpha
    (module struct
      type data = B_k_lessleader.dag_data
      type state = data Simulator.data PrivateAttack.state

      let description = Printf.sprintf "Bₖ/ll with k=%d" k
      let protocol = B_k_lessleader.protocol ~k
      let reward_function = reward

      include B_k_lessleader.PrivateAttack

      let node = PrivateAttack.withhold protocol.honest

      let apply_action v a state action =
        let tactic = tactic_of_policy ~k (fun _ -> action) in
        PrivateAttack.apply_tactic tactic v a state
      ;;
    end)
;;

let george ~alpha ~k ~reward =
  of_module
    ~alpha
    (module struct
      type data = George.dag_data
      type state = data Simulator.data PrivateAttack.state

      let description = Printf.sprintf "George with k=%d" k
      let protocol = George.protocol ~k
      let reward_function = reward

      include George.PrivateAttack

      let node = PrivateAttack.withhold protocol.honest

      let apply_action v a state action =
        let tactic = tactic_of_policy ~k (fun _ -> action) in
        PrivateAttack.apply_tactic tactic v a state
      ;;
    end)
;;

let default = bk ~k:51 ~alpha:0.25 ~reward:(B_k.constant_pow 1.)
