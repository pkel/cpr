open Cpr_lib

type network =
  | Simple10uniform
  | Simple2zero of { alpha : float }

let tag_network = function
  | Simple10uniform -> "simple10-uni"
  | Simple2zero _ -> "simple2-zero"
;;

let describe_network = function
  | Simple10uniform ->
    "10 nodes, compute 1..10, simple dissemination, uniform delay 0.6..1.4"
  | Simple2zero { alpha } ->
    Printf.sprintf
      "2 nodes, compute [%g,%g], simple dissemination, zero delay"
      alpha
      (1. -. alpha)
;;

let create_network = function
  | Simple10uniform ->
    let delay = Distributions.uniform ~lower:0.6 ~upper:1.4 in
    Network.homogeneous ~delay 10
    |> fun n ->
    { n with
      nodes =
        Array.mapi Network.(fun i x -> { x with compute = float_of_int (i + 1) }) n.nodes
    }
  | Simple2zero { alpha } ->
    let delay = Distributions.constant 0. in
    Network.
      { dissemination = Simple
      ; nodes =
          [| { compute = alpha; links = [ { dest = 1; delay } ] }
           ; { compute = 1. -. alpha; links = [ { dest = 0; delay } ] }
          |]
      }
;;

type protocol =
  | Nakamoto
  | B_k_lessleadership of { k : int }
  | George of { k : int }

let protocol_family = function
  | Nakamoto -> "nakamoto"
  | B_k_lessleadership _ -> "bk"
  | George _ -> "george"
;;

let pow_per_block = function
  | Nakamoto -> 1
  | B_k_lessleadership { k } | George { k } -> k
;;

let describe_protocol = function
  | Nakamoto -> "Nakamoto consensus"
  | B_k_lessleadership { k } ->
    "Bₖ with less leader modification and k=" ^ string_of_int k
  | George { k } -> "George's protocol with k=" ^ string_of_int k
;;

type incentive_scheme =
  | Constant
  | Discount
  | Punish
  | Hybrid

let tag_incentive_scheme = function
  | Constant -> "constant"
  | Discount -> "discount"
  | Punish -> "punish"
  | Hybrid -> "hybrid"
;;

let describe_incentive_scheme = function
  | Constant -> "1 per confirmed block, divided equally among confirmed pow"
  | Discount ->
    "max 1 per confirmed block, dk⁻² per pow solution (d ∊ 1..k = height since last \
     block)"
  | Punish ->
    "max 1 per confirmed block, k⁻¹ per pow solution on longest chain of votes"
  | Hybrid ->
    "max 1 per confirmed block, dk⁻² per pow solution on longest chain of votes (d \
     ∊ 1..k = height since last block)"
;;
