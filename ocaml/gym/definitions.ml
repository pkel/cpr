open Cpr_lib
open Cpr_protocols

let nakamoto ~reward =
  Engine.of_module
    (module struct
      open Nakamoto.SszAttack

      type data = Nakamoto.dag_data
      type state = data Simulator.data State.t

      let description = info
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
      type honest_state = env Dag.vertex

      let protocol = Nakamoto.protocol

      include Nakamoto.SszAttack

      type agent_state = env State.t
      type pre_action = env State.t
    end)
;;

let bk ~k ~reward =
  Engine.of_module
    (module struct
      open B_k.SszLikeAttack

      type data = B_k.dag_data
      type state = data Simulator.data State.t

      let protocol = B_k.protocol ~k
      let description = info
      let reward_function = reward
      let policies = Collection.map_to_list (fun e -> e.key, e.it) policies

      module Action = Action
      module Observation = Observation

      open Agent (struct
        let k = k
      end)

      let node v =
        let handler = prepare v in
        { (noop_node v) with handler }
      ;;

      let apply_action v a s x = apply v a s x
      let shutdown v a s = shutdown v a s
    end)
;;

let bk2 ~k =
  Engine2.of_module
    (module struct
      type data = B_k.dag_data
      type env = data Simulator.data
      type pow = Simulator.pow
      type honest_state = env Dag.vertex

      let protocol = B_k.protocol ~k

      include B_k.SszLikeAttack

      type agent_state = env State.t
      type pre_action = env State.t

      include Agent (struct
        let k = k
      end)
    end)
;;

let bk_ll ~k ~reward =
  Engine.of_module
    (module struct
      open B_k_lessleader.SszLikeAttack

      type data = B_k_lessleader.dag_data
      type state = data Simulator.data State.t

      let protocol = B_k_lessleader.protocol ~k
      let description = info
      let reward_function = reward
      let policies = Collection.map_to_list (fun e -> e.key, e.it) policies

      module Action = Action
      module Observation = Observation

      open Agent (struct
        let k = k
      end)

      let node v =
        let handler = prepare v in
        { (noop_node v) with handler }
      ;;

      let apply_action v a s x = apply v a s x
      let shutdown v a s = shutdown v a s
    end)
;;

let bk_ll2 ~k =
  Engine2.of_module
    (module struct
      type data = B_k_lessleader.dag_data
      type env = data Simulator.data
      type pow = Simulator.pow
      type honest_state = env Dag.vertex

      let protocol = B_k_lessleader.protocol ~k

      include B_k_lessleader.SszLikeAttack

      type agent_state = env State.t
      type pre_action = env State.t

      include Agent (struct
        let k = k
      end)
    end)
;;

let tailstorm ~k ~reward =
  Engine.of_module
    (module struct
      open Tailstorm.SszLikeAttack

      type data = Tailstorm.dag_data
      type state = data Simulator.data State.t

      let protocol = Tailstorm.protocol ~k
      let description = info
      let reward_function = reward
      let policies = Collection.map_to_list (fun e -> e.key, e.it) policies

      module Action = Action
      module Observation = Observation

      open Agent (struct
        let k = k
      end)

      let node v =
        let handler = prepare v in
        { (noop_node v) with handler }
      ;;

      let apply_action v a s x = apply v a s x
      let shutdown v a s = shutdown v a s
    end)
;;

let tailstorm2 ~k =
  Engine2.of_module
    (module struct
      type data = Tailstorm.dag_data
      type env = data Simulator.data
      type pow = Simulator.pow
      type honest_state = env Dag.vertex

      let protocol = Tailstorm.protocol ~k

      include Tailstorm.SszLikeAttack

      type agent_state = env State.t
      type pre_action = env State.t

      include Agent (struct
        let k = k
      end)
    end)
;;

let tailstorm_deprecated ~k ~reward =
  Engine.of_module
    (module struct
      open Tailstorm.PrivateAttack

      type data = Tailstorm.dag_data
      type nonrec state = data Simulator.data state

      let protocol = Tailstorm.protocol ~k
      let description = info
      let reward_function = reward
      let policies = Collection.map_to_list (fun e -> e.key, e.it) policies

      module Action = Action
      module Observation = Observation

      open Agent (struct
        let k = k
      end)

      let node v =
        let handler = prepare v in
        { (noop_node v) with handler }
      ;;

      let apply_action v a s x = apply v a s x
      let shutdown v a s = shutdown v a s
    end)
;;

let tailstorm_deprecated2 ~k =
  Engine2.of_module
    (module struct
      type data = Tailstorm.dag_data
      type env = data Simulator.data
      type pow = Simulator.pow
      type honest_state = env Dag.vertex

      let protocol = Tailstorm.protocol ~k

      include Tailstorm.PrivateAttack

      type agent_state = env state
      type pre_action = env state

      include Agent (struct
        let k = k
      end)
    end)
;;
