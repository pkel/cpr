open Cpr_lib
open Cpr_protocols
open Intf

let test : _ env =
  let k = 51
  and alpha = 1. /. 3. in
  let actions = Array.of_list B_k.actions in
  let params : Simulator.params =
    let delay = Distributions.constant 0. in
    { activations = -1
    ; network =
        Network.
          { dissemination = Simple
          ; nodes =
              [| { compute = alpha; links = [ { dest = 1; delay } ] }
               ; { compute = 1. -. alpha; links = [ { dest = 0; delay } ] }
              |]
          }
    ; activation_delay = 1.
    }
  in
  let init () =
    let open Simulator in
    let setup = all_honest params (B_k.protocol ~k) in
    let v, a, n = patch ~node:0 B_k.(strategic ~k noop_tactic) setup in
    init setup, (B_k.strategic_view v, a, n)
  in
  let rec skip_to_interaction state =
    let open Simulator in
    match dequeue state with
    | Some ev ->
      handle_event params state ev;
      if ev.node = 0 then () else skip_to_interaction state
    | None -> failwith "simulation should continue forever"
  in
  let create () =
    let state, attacker = init () in
    skip_to_interaction state;
    ref (state, attacker)
  and reset t =
    let state, attacker = init () in
    let _v, _a, (n : _ Simulator.node') = attacker in
    skip_to_interaction state;
    t := state, attacker;
    n.state
  in
  let step t ~action:i =
    let state, (v, a, (n : _ Simulator.node')) = !t in
    n.state <- B_k.apply_action ~k v a n.state actions.(i);
    skip_to_interaction state;
    (* TODO reward *)
    n.state, 0., true
  and numpy _ = Float.Array.of_list [ 42. ]
  and low = Float.Array.of_list [ 0. ]
  and high = Float.Array.of_list [ 100. ] in
  { n_actions = Array.length actions; create; reset; step; numpy; low; high }
;;
