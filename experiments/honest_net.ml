open Cpr_lib
open Cpr_protocols

module Task = struct
  type network = Simple10uniform

  let tag_network = function
    | Simple10uniform -> "simple10-uni"
  ;;

  let describe_network = function
    | Simple10uniform ->
      "10 nodes, compute 1..10, simple dissemination, uniform delay 0.6..1.4"
  ;;

  let create_network = function
    | Simple10uniform ->
      let delay = Distributions.uniform ~lower:0.6 ~upper:1.4 in
      Network.homogeneous ~delay 10
      |> fun n ->
      { n with
        nodes =
          Array.mapi
            Network.(fun i x -> { x with compute = float_of_int (i + 1) })
            n.nodes
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
      "max 1 per confirmed block, dk⁻² per pow solution (d ∊ 1..k = height since \
       last block)"
    | Punish ->
      "max 1 per confirmed block, k⁻¹ per pow solution on longest chain of votes"
    | Hybrid ->
      "max 1 per confirmed block, dk⁻² per pow solution on longest chain of votes"
  ;;

  type model =
    | M :
        { consensus : ('a Simulator.data, 'a, 'b, Simulator.pow) Protocol.protocol
        ; reward_functions : ('a Simulator.data, 'a) Protocol.reward_function list
        }
        -> model

  let model protocol incentive_schemes =
    let fail x =
      let msg =
        Printf.sprintf
          "protocol \"%s\" does not support incentive scheme \"%s\""
          (protocol_family protocol)
          (tag_incentive_scheme x)
      in
      failwith msg
    in
    match protocol with
    | Nakamoto ->
      M
        { consensus = Nakamoto.protocol
        ; reward_functions =
            List.map
              (function
                | Constant -> Nakamoto.constant 1.
                | x -> fail x)
              incentive_schemes
        }
    | B_k_lessleadership { k } ->
      M
        { consensus = B_k_lessleader.protocol ~k
        ; reward_functions =
            List.map
              (function
                | Constant -> B_k_lessleader.constant (1. /. float_of_int k)
                | x -> fail x)
              incentive_schemes
        }
    | George { k } ->
      M
        { consensus = George.protocol ~k
        ; reward_functions =
            (let reward = George.reward ~max_reward_per_block:1. ~k in
             List.map
               (function
                 | Constant -> reward ~punish:false ~discount:false
                 | Discount -> reward ~punish:false ~discount:true
                 | Punish -> reward ~punish:true ~discount:false
                 | Hybrid -> reward ~punish:true ~discount:true)
               incentive_schemes)
        }
  ;;

  type t =
    { network : network
    ; protocol : protocol
    ; incentive_schemes : incentive_scheme list
    ; activations : int
    ; activation_delay : float
    }

  let to_string t =
    Printf.sprintf
      "%s/k=%i/%gs/%s"
      (protocol_family t.protocol)
      (pow_per_block t.protocol)
      t.activation_delay
      (tag_network t.network)
  ;;
end

let networks = [ Task.Simple10uniform ]

let protocols =
  let k = [ 1; 2; 4; 8; 16; 32; 64; 128 ] in
  let open Task in
  List.concat
    [ [ Task.Nakamoto, [ Constant ] ]
    ; List.map (fun k -> Task.B_k_lessleadership { k }, [ Constant ]) k
    ; List.map (fun k -> Task.George { k }, [ Constant; Punish; Discount; Hybrid ]) k
    ]
;;

let block_intervals = [ 30.; 60.; 120.; 300.; 600. ]

let tasks activations =
  List.concat_map
    (fun network ->
      List.concat_map
        (fun (protocol, incentive_schemes) ->
          List.map
            (fun block_interval ->
              let open Task in
              { network
              ; incentive_schemes
              ; protocol
              ; activations
              ; activation_delay =
                  block_interval /. (Task.pow_per_block protocol |> float_of_int)
              })
            block_intervals)
        protocols)
    networks
;;

type row =
  { network : string
  ; network_description : string
  ; compute : float array
  ; protocol : string
  ; k : int
  ; protocol_description : string
  ; block_interval : float
  ; activation_delay : float
  ; number_activations : int
  ; activations : int array
  ; incentive_scheme : string
  ; incentive_scheme_description : string
  ; reward : float array
  }

let df_spec =
  let open Owl_dataframe in
  let array f arr = String (Array.to_list arr |> List.map f |> String.concat "|") in
  [| ("network", fun row -> String row.network)
   ; ("network_description", fun row -> String row.network_description)
   ; ("compute", fun row -> array string_of_float row.compute)
   ; ("protocol", fun row -> String row.protocol)
   ; ("k", fun row -> Int row.k)
   ; ("protocol_description", fun row -> String row.protocol_description)
   ; ("block_interval", fun row -> Float row.block_interval)
   ; ("activation_delay", fun row -> Float row.activation_delay)
   ; ("number_activations", fun row -> Int row.number_activations)
   ; ("activations", fun row -> array string_of_int row.activations)
   ; ("incentive_scheme", fun row -> String row.incentive_scheme)
   ; ("incentive_scheme_description", fun row -> String row.incentive_scheme_description)
   ; ("reward", fun row -> array string_of_float row.reward)
  |]
;;

let save_rows_as_tsv filename l =
  let df = Owl_dataframe.make (Array.map fst df_spec) in
  let record (row : row) =
    Array.map (fun (_, f) -> f row) df_spec |> Owl_dataframe.append_row df
  in
  List.iter record l;
  Owl_dataframe.to_csv ~sep:'\t' df filename
;;

let run task =
  let open Task in
  (* network stats *)
  let network = create_network task.network in
  let compute =
    let open Network in
    Array.map (fun x -> x.compute) network.nodes
  in
  (* simulate *)
  let open Simulator in
  let params =
    { network; activations = task.activations; activation_delay = task.activation_delay }
  in
  let (M m) = model task.protocol task.incentive_schemes in
  init params m.consensus
  |> loop params
  |> fun sim ->
  let activations = Array.map (fun (SNode x) -> x.n_activations) sim.nodes in
  Array.to_seq sim.nodes
  |> Seq.map (fun (SNode x) -> x.preferred x.state)
  |> Dag.common_ancestor' sim.global_view
  |> function
  | None -> failwith "no common ancestor found"
  | Some common_chain ->
    (* incentive stats *)
    List.map2
      (fun is rewardfn ->
        let reward = apply_reward_function rewardfn common_chain sim in
        { network = tag_network task.network
        ; network_description = describe_network task.network
        ; protocol = protocol_family task.protocol
        ; protocol_description = describe_protocol task.protocol
        ; k = pow_per_block task.protocol
        ; activation_delay = task.activation_delay
        ; number_activations = task.activations
        ; activations
        ; compute
        ; block_interval =
            task.activation_delay *. (pow_per_block task.protocol |> float_of_int)
        ; incentive_scheme = tag_incentive_scheme is
        ; incentive_scheme_description = describe_incentive_scheme is
        ; reward
        })
      task.incentive_schemes
      m.reward_functions
;;

let bar ~n_tasks =
  let open Progress.Line in
  list
    [ brackets (elapsed ())
    ; bar n_tasks
    ; count_to n_tasks
    ; parens (const "eta: " ++ eta n_tasks)
    ]
;;

let main n_activations n_cores filename =
  let tasks = tasks n_activations in
  let n_tasks = List.length tasks in
  let queue = ref tasks in
  let acc = ref [] in
  Printf.eprintf "Run %d simulations in parallel\n" n_cores;
  Progress.with_reporter (bar ~n_tasks) (fun progress ->
      Parany.run
        n_cores
        ~demux:(fun () ->
          match !queue with
          | [] -> raise Parany.End_of_input
          | hd :: tl ->
            queue := tl;
            hd)
        ~work:(fun task -> run task (*TODO log start; catch and marshal error/trace *))
        ~mux:(fun l ->
          progress 1;
          acc := l :: !acc (*TODO log error *)));
  let rows = List.concat (List.rev !acc) in
  save_rows_as_tsv filename rows
;;

open Cmdliner

let activations =
  let doc = "Number of proof-of-work activations simulated per output row." in
  let env = Arg.env_var "CPR_ACTIVATIONS" ~doc in
  Arg.(value & opt int 10000 & info [ "n"; "activations" ] ~env ~docv:"ACTIVATIONS" ~doc)
;;

let cores =
  let doc = "Number of simulation tasks run in parallel" in
  let env = Arg.env_var "CPR_CORES" ~doc in
  Arg.(value & opt int (Cpu.numcores ()) & info [ "p"; "cores" ] ~env ~docv:"CORES" ~doc)
;;

let filename =
  let doc = "Name of CSV output file (tab-separated)." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"OUTPUT" ~doc)
;;

let main_t = Term.(const main $ activations $ cores $ filename)

let info =
  let doc = "simulate various protocols in honest network" in
  Term.info "honest_net" ~doc
;;

let () = Term.exit @@ Term.eval (main_t, info)
