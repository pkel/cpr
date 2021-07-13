open Cpr_lib
open Cpr_protocols
open Models

module Task = struct
  type model =
    | M :
        { consensus : ('a Simulator.data, 'a, Simulator.pow) Protocol.protocol
        ; reward_functions : ('a Simulator.data, 'a) Protocol.reward_function list
        ; attacker :
            ('a Simulator.data, 'a) Protocol.context
            -> ('a Simulator.data, 'a, Simulator.pow) Protocol.node
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
    | Nakamoto -> failwith "protocol \"nakamoto\" does not support withholding"
    | B_k_lessleadership { k } ->
      M
        { consensus = B_k_lessleader.protocol ~k
        ; reward_functions =
            List.map
              (function
                | Constant -> B_k_lessleader.constant (1. /. float_of_int k)
                | x -> fail x)
              incentive_schemes
        ; attacker = B_k_lessleader.selfish ~k
        }
    | George _ -> failwith "protocol \"george\" does not support withholding"
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

let task =
  let open Task in
  { network = Simple2zero { alpha = 0.33 }
  ; incentive_schemes = [ Constant ]
  ; protocol = B_k_lessleadership { k = 16 }
  ; activations = 200
  ; activation_delay = 1.
  }
;;

let run task =
  let open Task in
  (* network stats *)
  let network = create_network task.network in
  (* simulate *)
  let open Simulator in
  let params =
    { network; activations = task.activations; activation_delay = task.activation_delay }
  in
  let (M m) = model task.protocol task.incentive_schemes in
  let deviations =
    let a = Array.make (Array.length network.nodes) None in
    a.(0) <- Some m.attacker;
    a
  in
  Random.self_init ();
  init ~deviations params m.consensus
  |> loop params
  |> fun sim ->
  Dag.dot
    stdout
    sim.global_view
    (fun n ->
      let d = Dag.data n in
      Printf.sprintf
        "%s | %.2f%s"
        (match d.appended_by with
        | None -> "root"
        | Some 0 -> "a"
        | Some _ -> "d")
        d.appended_at
        (match Dag.children sim.global_view n with
        | [] -> " | o"
        | _ -> ""))
    (Dag.roots sim.dag)
;;

let () = run task
