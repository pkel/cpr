open Cpr_lib

(** {1} protocols instantiated for a specific simulator *)

include struct
  open Simulator

  type t = P : ('dag_data data, 'dag_data, pow, 'node_state) protocol -> t

  open Cpr_protocols

  let nakamoto = P Nakamoto.protocol
  let bk ~k = P (B_k.protocol ~k)
  let bk_lessleader ~k = P (B_k_lessleader.protocol ~k)
  let george ~k = P (George.protocol ~k)
end

(** {1} simulated environments *)

type network =
  | CliqueUniform10
  | TwoAgentsZero of { alpha : float }

let tag_network = function
  | CliqueUniform10 -> "clique-uniform-10"
  | TwoAgentsZero _ -> "two-agents-zero"
;;

let describe_network = function
  | CliqueUniform10 ->
    "10 nodes, compute 1..10, simple dissemination, uniform delay 0.6..1.4"
  | TwoAgentsZero { alpha } -> Printf.sprintf "2 nodes, alpha=%g, zero delay" alpha
;;

type scenario =
  | AllHonest
  | FirstSelfish

let tag_scenario = function
  | AllHonest -> "all-honest"
  | FirstSelfish -> "first-selfish"
;;

let describe_scenario = function
  | AllHonest -> "all nodes follow the protocol"
  | FirstSelfish -> "first node acts selfishly, other nodes follow the protocol"
;;

type model =
  { network : network
  ; scenario : scenario
  ; activations : int
  ; activation_delay : float
  }

type setup =
  { model : model
  ; params : Simulator.params
  ; network : Network.t
  ; attacker : int option
  }

let setup (m : model) =
  let network, attacker =
    match m.network with
    | CliqueUniform10 ->
      let delay = Distributions.uniform ~lower:0.6 ~upper:1.4 in
      Network.homogeneous ~delay 10
      |> fun n ->
      ( { n with
          nodes =
            Array.mapi
              Network.(fun i x -> { x with compute = float_of_int (i + 1) })
              n.nodes
        }
      , None )
    | TwoAgentsZero { alpha } ->
      let delay = Distributions.constant 0. in
      ( Network.
          { dissemination = Simple
          ; nodes =
              [| { compute = alpha; links = [ { dest = 1; delay } ] }
               ; { compute = 1. -. alpha; links = [ { dest = 0; delay } ] }
              |]
          }
      , Some 0 )
  in
  let params =
    Simulator.
      { network; activations = m.activations; activation_delay = m.activation_delay }
  in
  { model = m; params; network; attacker }
;;
