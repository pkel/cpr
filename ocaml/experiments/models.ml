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

let honest_clique ~activation_delay ~n protocol =
  let propagation_delay = Distributions.uniform ~lower:0.5 ~upper:1.5 in
  let net = Network.homogeneous ~activation_delay ~propagation_delay n in
  let net =
    { net with
      nodes =
        Array.mapi
          Network.(fun i x -> { x with compute = float_of_int (i + 1) })
          net.nodes
    }
  in
  let it () = Simulator.all_honest net protocol |> Simulator.init in
  ( Collection.
      { it
      ; key = Printf.sprintf "honest-clique-%i" n
      ; info =
          (* TODO: describe delay distribution *)
          Printf.sprintf
            "%i nodes, compute 1..%i, simple dissemination, uniform propagation delay \
             0.5 .. 1.5"
            n
            n
      }
  , net )
;;

let two_agents ~alpha protocol attack =
  let delay = Distributions.constant 0. in
  let net =
    Network.
      { dissemination = Simple
      ; nodes =
          [| { compute = alpha; links = [ { dest = 1; delay } ] }
           ; { compute = 1. -. alpha; links = [ { dest = 0; delay } ] }
          |]
      ; activation_delay = 1.
      }
  in
  let it () =
    let sim = Simulator.all_honest net protocol in
    let () =
      let (Node n) = attack.Collection.it in
      Simulator.patch ~node:0 n sim |> ignore
    in
    Simulator.init sim
  in
  ( Collection.
      { it
      ; key = Printf.sprintf "two-agents-%g" alpha
      ; info =
          Printf.sprintf "2 nodes, alpha=%g, no propagation delays" alpha
          (* TODO: describe delay distribution *)
      }
  , net )
;;

(* Selfish mining literature uses a single gamma parameter to model network connectivity.
   In the selfish mining model, the attacker sees all defender blocks as soon as they are
   mined. If the attacker decides to release a block of the same height (MATCH action),
   gamma of the defender compute adopts the attackers block. In real networks (and this
   simulator) honest nodes adopt the block first received. Hence, modelling gamma boils
   down to reordering messages. The following function creates networks that exhibit a
   certain gamma. *)
let selfish_mining ?(msg_delay = 1. /. 10000.) ~defenders ~alpha gamma protocol attack =
  let defender_compute =
    if defenders < 1
    then raise (Invalid_argument "defenders must be greater zero.")
    else (1. -. alpha) /. float_of_int defenders
  in
  let attacker_msg_delay =
    if gamma > 1. -. defender_compute
    then
      raise (Invalid_argument "gamma must not be greater ( 1 - (1 - alpha) / defenders )")
    else (
      let gamma' = gamma +. defender_compute in
      let lower = msg_delay *. (1. -. gamma')
      and upper = msg_delay *. (2. -. gamma') in
      Distributions.uniform ~lower ~upper)
  in
  let net =
    let n = defenders + 1 in
    let open Network in
    let links src =
      List.filter
        (fun l -> l.dest <> src)
        (if src = 0
        then
          (* attacker messages take random time to model gamma *)
          List.init n (fun dest -> { dest; delay = attacker_msg_delay })
        else
          List.init n (fun dest ->
              if dest = 0
              then
                (* attacker receives messages immediately *)
                { dest; delay = Distributions.constant 0. }
              else
                (* defender messages take msg_delay time *)
                { dest; delay = Distributions.constant msg_delay }))
    in
    Network.
      { dissemination = Simple
      ; nodes =
          Array.init n (fun i ->
              let links = links i in
              if i = 0
              then (* attacker *)
                { compute = alpha; links }
              else (* defender *) { compute = defender_compute; links })
      ; activation_delay = 1.
      }
  in
  let it () =
    let sim = Simulator.all_honest net protocol in
    let () =
      let (Node n) = attack.Collection.it in
      Simulator.patch ~node:0 n sim |> ignore
    in
    Simulator.init sim
  in
  ( Collection.
      { it
      ; key = Printf.sprintf "gamma-%g-alpha-%g" gamma alpha
      ; info =
          Printf.sprintf
            "1 attacker, alpha=%g, %i symmetric defenders, constant propagation delays \
             modeling gamma=%g. with defender message delay %g)"
            alpha
            defenders
            gamma
            msg_delay
      }
  , net )
;;
