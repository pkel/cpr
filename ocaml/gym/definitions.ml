open Cpr_lib
open Cpr_protocols

let nakamoto =
  Engine.of_module
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

let bk ~k =
  Engine.of_module
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

let bk_ll ~k =
  Engine.of_module
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

let tailstorm ~k =
  Engine.of_module
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

let tailstorm_deprecated ~k =
  Engine.of_module
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
