open Cpr_lib
open Cpr_protocols

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

type protocol =
  | Nakamoto
  | B_k of { k : int }
  | B_k_lessleadership of { k : int }
  | George of { k : int }

let protocol_family = function
  | Nakamoto -> "nakamoto"
  | B_k _ -> "bk"
  | B_k_lessleadership _ -> "bk+ll"
  | George _ -> "george"
;;

let pow_per_block = function
  | Nakamoto -> 1
  | B_k { k } | B_k_lessleadership { k } | George { k } -> k
;;

let describe_protocol = function
  | Nakamoto -> "Nakamoto consensus"
  | B_k { k } -> "Bₖ with k=" ^ string_of_int k
  | B_k_lessleadership { k } ->
    "Bₖ with less leader modification and k=" ^ string_of_int k
  | George { k } -> "George's protocol with k=" ^ string_of_int k
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

type strategy =
  | Honest
  | SelfishSimple
  | SelfishAdvanced
  | NumHonest
  | NumSelfishSimple
  | NumSelfishAdvanced

let tag_strategy = function
  | Honest -> "honest"
  | SelfishSimple -> "selfish-simple"
  | SelfishAdvanced -> "selfish-advanced"
  | NumHonest -> "numeric-honest"
  | NumSelfishSimple -> "numeric-selfish-simple"
  | NumSelfishAdvanced -> "numeric-selfish-advanced"
;;

let describe_strategy = function
  | Honest -> "honest"
  | SelfishSimple -> "withhold until strong block is found"
  | SelfishAdvanced -> "withhold until defender finds strong block"
  | NumHonest -> "honest strategy implemented on RL/Gym view"
  | NumSelfishSimple -> "selfish-simple strategy implemented on RL/Gym view"
  | NumSelfishAdvanced -> "selfish-advanced strategy implemented on RL/Gym view"
;;

type task =
  { network : network
  ; protocol : protocol
  ; scenario : scenario
  ; strategy : strategy
  ; activations : int
  ; activation_delay : float
  }

type 'a deviation =
  | Deviation :
      (('a Simulator.data, 'a) local_view
       -> ('a Simulator.data, 'a, Simulator.pow, 'state) node)
      -> 'a deviation

type setup =
  | S :
      { task : task
      ; params : Simulator.params
      ; network : Network.t
      ; protocol : ('a Simulator.data, 'a, Simulator.pow, 'state) Cpr_lib.protocol
      ; deviations : (int * 'a deviation) list
      }
      -> setup

let setup t =
  let network =
    match t.network with
    | CliqueUniform10 ->
      let delay = Distributions.uniform ~lower:0.6 ~upper:1.4 in
      Network.homogeneous ~delay 10
      |> fun n ->
      { n with
        nodes =
          Array.mapi
            Network.(fun i x -> { x with compute = float_of_int (i + 1) })
            n.nodes
      }
    | TwoAgentsZero { alpha } ->
      let delay = Distributions.constant 0. in
      Network.
        { dissemination = Simple
        ; nodes =
            [| { compute = alpha; links = [ { dest = 1; delay } ] }
             ; { compute = 1. -. alpha; links = [ { dest = 0; delay } ] }
            |]
        }
  in
  let params =
    Simulator.
      { network; activations = t.activations; activation_delay = t.activation_delay }
  and deviations of_strategy =
    match t.scenario with
    | AllHonest -> []
    | FirstSelfish -> [ 0, of_strategy t.strategy ]
  in
  match t.protocol with
  | Nakamoto ->
    let open Nakamoto in
    let deviations =
      deviations (function
          | Honest -> Deviation protocol.honest
          | SelfishAdvanced -> Deviation PrivateAttack.(attack' selfish_policy')
          | NumHonest -> Deviation PrivateAttack.(attack honest_policy)
          | NumSelfishAdvanced -> Deviation PrivateAttack.(attack selfish_policy)
          | x ->
            let m =
              Printf.sprintf
                "protocol %s does not support attack strategy %s"
                (protocol_family t.protocol)
                (tag_strategy x)
            in
            raise (Invalid_argument m))
    in
    S { task = t; params; network; protocol; deviations }
  | B_k { k } ->
    let open B_k in
    let protocol = protocol ~k in
    let deviations =
      deviations (function
          | Honest -> Deviation protocol.honest
          | SelfishAdvanced -> Deviation PrivateAttack.(attack' selfish_policy' ~k)
          | NumHonest -> Deviation PrivateAttack.(attack honest_policy ~k)
          | NumSelfishAdvanced -> Deviation PrivateAttack.(attack selfish_policy ~k)
          | x ->
            let m =
              Printf.sprintf
                "protocol %s does not support attack strategy %s"
                (protocol_family t.protocol)
                (tag_strategy x)
            in
            raise (Invalid_argument m))
    in
    S { task = t; params; network; protocol; deviations }
  | B_k_lessleadership { k } ->
    let open B_k_lessleader in
    let protocol = protocol ~k in
    let deviations =
      deviations (function
          | Honest -> Deviation protocol.honest
          | SelfishAdvanced -> Deviation PrivateAttack.(attack' selfish_policy' ~k)
          | NumHonest -> Deviation PrivateAttack.(attack honest_policy ~k)
          | NumSelfishAdvanced -> Deviation PrivateAttack.(attack selfish_policy ~k)
          | x ->
            let m =
              Printf.sprintf
                "protocol %s does not support attack strategy %s"
                (protocol_family t.protocol)
                (tag_strategy x)
            in
            raise (Invalid_argument m))
    in
    S { task = t; params; network; protocol; deviations }
  | George { k } ->
    let open George in
    let protocol = protocol ~k in
    let deviations =
      deviations (function
          | Honest -> Deviation protocol.honest
          | SelfishAdvanced -> Deviation PrivateAttack.(attack' selfish_policy' ~k)
          | NumHonest -> Deviation PrivateAttack.(attack honest_policy ~k)
          | NumSelfishAdvanced -> Deviation PrivateAttack.(attack selfish_policy ~k)
          | x ->
            let m =
              Printf.sprintf
                "protocol %s does not support attack strategy %s"
                (protocol_family t.protocol)
                (tag_strategy x)
            in
            raise (Invalid_argument m))
    in
    S { task = t; params; network; protocol; deviations }
;;
