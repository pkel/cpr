open Cpr_lib
open Cpr_protocols

type 'a with_tag_and_description =
  { tag : string
  ; description : string
  ; it : 'a
  }

let networks =
  [ { tag = "simple10-uni"
    ; description =
        "10 nodes, compute 1..10, simple dissemination, uniform delay 0.6..1.4"
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
      ; k : int (* pow per block *)
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
        ; k = 1
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
  { tag = "bk"
  ; description = "Bₖ with less leader modification and k=" ^ string_of_int k
  ; it =
      P
        { consensus = B_k_lessleader.protocol ~k
        ; k
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
  { tag = "george"
  ; description = "George's protocol with k=" ^ string_of_int k
  ; it =
      P
        { consensus = George.protocol ~k
        ; k
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

let block_intervals = [ 30.; 60.; 120.; 300.; 600. ]

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

let df =
  Owl_dataframe.make
    [| "network"
     ; "network-description"
     ; "compute"
     ; "protocol"
     ; "k"
     ; "protocol-description"
     ; "block-interval"
     ; "activation-delay"
     ; "n-activations"
     ; "incentive-scheme"
     ; "incentive-scheme-description"
     ; "reward"
    |]
;;

let record ~params ~network ~compute ~protocol ~block_interval ~incentive_scheme ~reward =
  let open Simulator in
  let open Owl_dataframe in
  let (P protocol') = protocol.it in
  let array arr =
    String (Array.to_list arr |> List.map string_of_float |> String.concat "|")
  in
  append_row
    df
    [| String network.tag
     ; String network.description
     ; array compute
     ; String protocol.tag
     ; Int protocol'.k
     ; String protocol.description
     ; Float block_interval
     ; Float params.activation_delay
     ; Int params.n_activations
     ; String incentive_scheme.tag
     ; String incentive_scheme.description
     ; array reward
    |]
;;

let run n_activations (network, protocol, block_interval) =
  let (P p) = protocol.it in
  let () =
    Printf.eprintf "%s/k=%i/%gs/%s\n%!" protocol.tag p.k block_interval network.tag
  in
  (* network stats *)
  let compute =
    let open Network in
    Array.map (fun x -> x.compute) network.it.nodes
  in
  (* simulate *)
  let open Simulator in
  let params =
    { network = network.it
    ; n_activations
    ; activation_delay = block_interval /. float_of_int p.k
    }
  in
  init params p.consensus
  |> loop params
  |> fun sim ->
  Array.to_seq sim.nodes
  |> Seq.map (fun x -> p.consensus.head x.state)
  |> Dag.common_ancestor' sim.global_view
  |> function
  | None -> failwith "no common ancestor found"
  | Some common_chain ->
    (* incentive stats *)
    List.iter
      (fun is ->
        let rewardfn = is.it in
        let reward = Array.make (Array.length sim.nodes) 0. in
        rewardfn
          sim.global_view
          (fun x -> x.value)
          (fun x -> x.appended_by)
          common_chain
          reward;
        record
          ~params
          ~compute
          ~network
          ~protocol
          ~block_interval
          ~incentive_scheme:is
          ~reward)
      p.incentive_schemes
;;

let main n_activations filename =
  Printf.eprintf "Simulate %d configurations...\n%!" (List.length simulations);
  List.iter (run n_activations) simulations;
  Owl_dataframe.to_csv ~sep:'\t' df filename
;;

open Cmdliner

let n_activations =
  let doc = "Number of proof-of-work activations simulated per output row." in
  let env = Arg.env_var "CPR_ACTIVATIONS" ~doc in
  Arg.(value & opt int 10000 & info [ "n" ] ~env ~docv:"ACTIVATIONS" ~doc)
;;

let filename =
  let doc = "Name of CSV output file." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"OUTPUT" ~doc)
;;

let main_t = Term.(const main $ n_activations $ filename)

let info =
  let doc = "simulate various protocols in honest network" in
  Term.info "honest_net" ~doc
;;

let () = Term.exit @@ Term.eval (main_t, info)
