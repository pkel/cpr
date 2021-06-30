open Cpr_lib
open Cpr_protocols
open Owl_base

let print_farray arr =
  let len = Array.length arr in
  print_char '[';
  for i = 0 to len - 2 do
    Printf.printf "%.3g" arr.(i);
    print_char ','
  done;
  Printf.printf "%.3g" arr.(len - 1);
  print_char ']'
;;

let gini arr =
  let n = Array.length arr in
  let sum = ref 0. in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      sum := Maths.abs (arr.(i) -. arr.(j)) +. !sum
    done
  done;
  let denominator = 2. *. Maths.sqr (float_of_int n) *. Stats.mean arr in
  !sum /. denominator
;;

let print_stats arr =
  print_farray arr;
  print_endline "";
  Printf.printf "sum:      %.4g\n" (Stats.sum arr);
  Printf.printf "median:   %.4g\n" (Stats.median arr);
  Printf.printf "mean:     %.4g\n" (Stats.mean arr);
  Printf.printf "std.dev.: %.4g\n" (Stats.std arr);
  Printf.printf "ginicoef: %.4g\n%!" (gini arr)
;;

(* execute protocol, return common ancestor of preferred heads *)
let run params (protocol : _ Protocol.protocol) (rewardfn : _ Protocol.reward_function) =
  let open Simulator in
  (* opening stats *)
  let compute = Array.map Network.(fun x -> x.compute) params.network.nodes in
  let sumcomp = Stats.sum compute in
  print_endline "+++ relative compute";
  let relcomp = Array.map (fun x -> x /. sumcomp) compute in
  print_stats relcomp;
  (* simulate *)
  init params protocol
  |> loop params
  |> fun sim ->
  Array.to_seq sim.nodes
  |> Seq.map (fun x -> protocol.head x.state)
  |> Dag.common_ancestor' sim.global_view
  |> function
  | None -> failwith "no common ancestor found"
  | Some common_chain ->
    (* closing stats *)
    print_endline "+++ absolute reward";
    let reward = Array.make (Array.length sim.nodes) 0. in
    rewardfn
      sim.global_view
      (fun x -> x.value)
      (fun x -> x.appended_by)
      common_chain
      reward;
    print_stats reward;
    print_endline "+++ relative reward";
    let sumrew = Stats.sum reward in
    let relrew = Array.map (fun x -> x /. sumrew) reward in
    print_stats relrew;
    print_endline "+++ efficiency = relative reward / relative compute";
    let efficiency = Array.map2 (fun rew comp -> rew /. comp) relrew relcomp in
    print_stats efficiency
;;

let network =
  let delay = Distributions.uniform ~lower:0.6 ~upper:1.4 in
  Network.homogeneous ~delay 10
  |> fun n ->
  { n with
    nodes =
      Array.mapi Network.(fun i x -> { x with compute = float_of_int (i + 1) }) n.nodes
  }
;;

let () =
  Random.self_init ();
  let open Simulator in
  print_endline "=== Nakamoto";
  print_endline "--- fully connected network with homogeneous uniform delays";
  print_endline "--- all nodes honest";
  print_endline "--- activation delay = 8 expected message delays";
  print_endline "--- 1 000 000 activations";
  print_endline "--- reward: 1 per block confirmed";
  run
    { network; n_activations = 1000000; activation_delay = 8. }
    Nakamoto.protocol
    (Nakamoto.constant_reward 1.);
  print_endline "=== Bₖ with less leader modification, k=10";
  print_endline "--- fully connected network with homogeneous uniform delays";
  print_endline "--- all nodes honest";
  print_endline "--- activation delay = 8 expected message delays";
  print_endline "--- 1 000 000 activations";
  print_endline "--- reward: 1 per pow confirmed";
  run
    { network; n_activations = 1000000; activation_delay = 8. }
    (B_k_lessleader.protocol ~k:10)
    (B_k_lessleader.constant_reward_per_pow 1.);
  print_endline "=== Bₖ with less leader modification, k=100";
  print_endline "--- fully connected network with homogeneous uniform delays";
  print_endline "--- all nodes honest";
  print_endline "--- activation delay = 8 expected message delays";
  print_endline "--- 1 000 000 activations";
  print_endline "--- reward: 1 per pow confirmed";
  run
    { network; n_activations = 1000000; activation_delay = 8. }
    (B_k_lessleader.protocol ~k:100)
    (B_k_lessleader.constant_reward_per_pow 1.);
  print_endline "=== Nakamoto";
  print_endline "--- fully connected network with homogeneous uniform delays";
  print_endline "--- all nodes honest";
  print_endline "--- activation delay = 1 expected message delay";
  print_endline "--- 1 000 000 activations";
  print_endline "--- reward: 1 per block confirmed";
  run
    { network; n_activations = 1000000; activation_delay = 1. }
    Nakamoto.protocol
    (Nakamoto.constant_reward 1.);
  print_endline "=== Bₖ with less leader modification, k=10";
  print_endline "--- fully connected network with homogeneous uniform delays";
  print_endline "--- all nodes honest";
  print_endline "--- activation delay = 1 expected message delay";
  print_endline "--- 1 000 000 activations";
  print_endline "--- reward: 1 per pow confirmed";
  run
    { network; n_activations = 1000000; activation_delay = 1. }
    (B_k_lessleader.protocol ~k:10)
    (B_k_lessleader.constant_reward_per_pow 1.);
  print_endline "=== Bₖ with less leader modification, k=100";
  print_endline "--- fully connected network with homogeneous uniform delays";
  print_endline "--- all nodes honest";
  print_endline "--- activation delay = 1 expected message delay";
  print_endline "--- 1 000 000 activations";
  print_endline "--- reward: 1 per pow confirmed";
  run
    { network; n_activations = 1000000; activation_delay = 1. }
    (B_k_lessleader.protocol ~k:100)
    (B_k_lessleader.constant_reward_per_pow 1.)
;;
