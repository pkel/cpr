open Cpr_lib
open Cpr_protocols

let nakamoto ~reward =
  Engine.of_module
    (module struct
      open Nakamoto.PrivateAttack

      type data = Nakamoto.dag_data
      type state = data Simulator.data State.t

      let description = "Nakamoto"
      let protocol = Nakamoto.protocol
      let reward_function = reward

      let node v =
        let handler = prepare v in
        { (noop_node v) with handler }
      ;;

      let policies = Collection.map_to_list (fun e -> e.key, e.it) policies

      module Action = Action
      module Observation = Observation

      let apply_action v a s x = apply v a s x
      let shutdown = shutdown
    end)
;;

let nakamoto2 =
  Engine2.of_module
    (module struct
      type data = Nakamoto.dag_data
      type env = data Simulator.data
      type pow = Simulator.pow
      type honest_state = data Simulator.data Dag.vertex

      let protocol = Nakamoto.protocol
      let description = "Nakamoto"

      include Nakamoto.PrivateAttack

      type agent_state = env State.t
      type pre_action = env State.t
    end)
;;

let bk ~k ~reward =
  Engine.of_module
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

      let shutdown _v _a state =
        (* TODO: implement shutdown for non-Nakamoto protocols *) state
      ;;
    end)
;;

let bk_ll ~k ~reward =
  Engine.of_module
    (module struct
      open B_k_lessleader.PrivateAttack

      type data = B_k_lessleader.dag_data
      type state = data Simulator.data State.t

      let description = Printf.sprintf "Bₖ/ll with k=%d" k
      let protocol = B_k_lessleader.protocol ~k
      let reward_function = reward
      let node = staging_agent ~k
      let policies = Collection.map_to_list (fun e -> e.key, e.it) Policies.collection

      module Action = Action
      module Observation = Observation

      let apply_action v a s x = interpret_action ~k v s x |> conclude_action ~k v a
      let shutdown = shutdown ~k
    end)
;;

let george ~k ~reward =
  Engine.of_module
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

      let shutdown _v _a state =
        (* TODO: implement shutdown for non-Nakamoto protocols *) state
      ;;
    end)
;;
