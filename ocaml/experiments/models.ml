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

let honest_clique ~n protocol params =
  let delay = Distributions.uniform ~lower:0.5 ~upper:1.5 in
  let net = Network.homogeneous ~delay n in
  let net =
    { net with
      nodes =
        Array.mapi
          Network.(fun i x -> { x with compute = float_of_int (i + 1) })
          net.nodes
    }
  in
  let it () = Simulator.all_honest params net protocol |> Simulator.init in
  Collection.
    { it
    ; key = Printf.sprintf "honest-clique-%i" n
    ; info =
        (* TODO: describe delay distribution *)
        Printf.sprintf
          "%i nodes, compute 1..%i, simple dissemination, uniform propagation delay 0.5 \
           .. 1.5"
          n
          n
    }
;;

let two_agents ~alpha protocol attack params =
  let delay = Distributions.constant 0. in
  let net =
    Network.
      { dissemination = Simple
      ; nodes =
          [| { compute = alpha; links = [ { dest = 1; delay } ] }
           ; { compute = 1. -. alpha; links = [ { dest = 0; delay } ] }
          |]
      }
  in
  let it () =
    let sim = Simulator.all_honest params net protocol in
    let () =
      let (Node n) = attack.Collection.it in
      Simulator.patch ~node:0 n sim |> ignore
    in
    Simulator.init sim
  in
  Collection.
    { it
    ; key = Printf.sprintf "two-agents-%g" alpha
    ; info =
        Printf.sprintf "2 nodes, alpha=%g, no propagation delays" alpha
        (* TODO: describe delay distribution *)
    }
;;
