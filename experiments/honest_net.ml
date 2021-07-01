open Cpr_lib
open Cpr_protocols
open Owl_base

type 'a with_tag_and_description =
  { tag : string
  ; description : string
  ; it : 'a
  }

let networks =
  [ { tag = "simple10-uni"
    ; description =
        "10 nodes; compute 1..10; simple dissemination; uniform delay 0.6..1.4"
    ; it =
        (let delay = Distributions.uniform ~lower:0.6 ~upper:1.4 in
         Network.homogeneous ~delay 10
         |> fun n ->
         { n with
           nodes =
             Array.mapi
               Network.(fun i x -> { x with compute = float_of_int (i + 1) })
               n.nodes
         })
    }
  ]
;;

type protocol =
  | P :
      { consensus : ('a Simulator.data, 'a, 'b, Simulator.pow) Protocol.protocol
      ; pow_per_block : int
      ; incentive_schemes :
          ('a Simulator.data, 'a) Protocol.reward_function with_tag_and_description list
      }
      -> protocol

let nakamoto =
  { tag = "nakamoto"
  ; description = "Nakamoto consensus"
  ; it =
      P
        { consensus = Nakamoto.protocol
        ; pow_per_block = 1
        ; incentive_schemes =
            [ { tag = "constant"
              ; description = "1 per confirmed block"
              ; it = Nakamoto.constant_reward 1.
              }
            ]
        }
  }
;;

let bk k =
  { tag = "bk" ^ string_of_int k
  ; description = "Bₖ with less leader modification and k=" ^ string_of_int k
  ; it =
      P
        { consensus = B_k_lessleader.protocol ~k
        ; pow_per_block = k
        ; incentive_schemes =
            [ { tag = "constant"
              ; description =
                  "1 per confirmed block, split equally between referenced votes"
              ; it = B_k_lessleader.constant_reward_per_pow (1. /. float_of_int k)
              }
            ]
        }
  }
;;

let george k =
  { tag = "george" ^ string_of_int k
  ; description = "George's protocol with k=" ^ string_of_int k
  ; it =
      P
        { consensus = George.protocol ~k
        ; pow_per_block = k
        ; incentive_schemes =
            [ { tag = "constant"
              ; description =
                  "1 per confirmed block, split equally between referenced pow solutions"
              ; it = George.constant_reward_per_pow ~reward_per_block:1. ~k
              }
            ; { tag = "discount"
              ; description =
                  "max 1 per confirmed block, discount for non-linearity, split equally \
                   between referenced pow solutions"
              ; it = George.discount_vote_depth ~max_reward_per_block:1. ~k
              }
            ; { tag = "punish"
              ; description =
                  "max 1 per confirmed block, 1/k per pow solution on the longest chain \
                   of pow solutions"
              ; it = George.punish_nonlinear ~max_reward_per_block:1. ~k
              }
            ]
        }
  }
;;

let protocols =
  let k = [ 1; 2; 4; 8; 16; 32; 64; 128 ] in
  List.concat [ [ nakamoto ]; List.map bk k; List.map george k ]
;;

let block_intervals = [ 30.; 60.; 300.; 600. ]
let n_activations = 100 * 1000

let simulations =
  List.concat_map
    (fun network ->
      List.concat_map
        (fun protocol ->
          List.map
            (fun block_interval -> network, protocol, block_interval)
            block_intervals)
        protocols)
    networks
;;

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

let run (network, protocol, block_interval) =
  let () =
    Printf.printf "### %s/%gs/%s\n" protocol.tag block_interval network.tag;
    Printf.printf "Network: %s\n" network.description;
    Printf.printf "Protocol: %s\n" protocol.description;
    Printf.printf "Block interval: %g\n" block_interval
  and network = network.it in
  let (P p) = protocol.it in
  (* network stats *)
  let compute =
    let open Network in
    Array.map (fun x -> x.compute) network.nodes
  in
  let sumcomp = Stats.sum compute in
  print_endline "--- relative compute";
  let relcomp = Array.map (fun x -> x /. sumcomp) compute in
  print_stats relcomp;
  (* simulate *)
  let open Simulator in
  let protocol = p.consensus
  and params =
    { network
    ; n_activations
    ; activation_delay = block_interval /. float_of_int p.pow_per_block
    }
  in
  init params protocol
  |> loop params
  |> fun sim ->
  Array.to_seq sim.nodes
  |> Seq.map (fun x -> protocol.head x.state)
  |> Dag.common_ancestor' sim.global_view
  |> function
  | None -> failwith "no common ancestor found"
  | Some common_chain ->
    (* incentive stats *)
    List.iter
      (fun is ->
        Printf.printf "+++ Reward: %s\n" is.description;
        print_endline "--- absolute reward";
        let rewardfn = is.it in
        let reward = Array.make (Array.length sim.nodes) 0. in
        rewardfn
          sim.global_view
          (fun x -> x.value)
          (fun x -> x.appended_by)
          common_chain
          reward;
        print_stats reward;
        print_endline "--- relative reward";
        let sumrew = Stats.sum reward in
        let relrew = Array.map (fun x -> x /. sumrew) reward in
        print_stats relrew;
        print_endline "--- efficiency = relative reward / relative compute";
        let efficiency = Array.map2 (fun rew comp -> rew /. comp) relrew relcomp in
        print_stats efficiency)
      p.incentive_schemes
;;

let () =
  Printf.eprintf "Simulate %d configurations...\n%!" (List.length simulations);
  List.iter run simulations
;;
