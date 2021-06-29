open Cpr_lib
open Cpr_protocols

let print_farray arr =
  let len = Float.Array.length arr in
  print_char '[';
  for i = 0 to len - 2 do
    print_float (Float.Array.get arr i);
    print_char ','
  done;
  print_float (Float.Array.get arr (len - 1));
  print_char ']'
;;

(* execute protocol, return common ancestor of preferred heads *)
let run params (protocol : _ Protocol.protocol) (rewardfn : _ Protocol.reward_function) =
  let open Simulator in
  init params protocol
  |> loop params
  |> fun sim ->
  Array.to_seq sim.nodes
  |> Seq.map (fun x -> protocol.head x.state)
  |> Dag.common_ancestor' sim.global_view
  |> function
  | None -> failwith "no common ancestor found"
  | Some common_chain ->
    let reward = Float.Array.make (Array.length sim.nodes) 0. in
    rewardfn
      sim.global_view
      (fun x -> x.value)
      (fun x -> x.appended_by)
      common_chain
      reward;
    print_farray reward;
    print_endline ""
;;

let network =
  let delay = Distributions.uniform ~lower:0.6 ~upper:1.4 in
  Network.homogeneous ~delay 10
;;

let () =
  let open Simulator in
  Random.self_init ();
  run
    { network; n_activations = 10000; activation_delay = 8. }
    Nakamoto.protocol
    (Nakamoto.constant_reward 1.)
;;
