open Cpr_lib

let honest_clique ~activation_delay ~n protocol =
  let propagation_delay = Distributions.uniform ~lower:0.5 ~upper:1.5 in
  let net = Network.T.symmetric_clique ~activation_delay ~propagation_delay n in
  let net =
    { net with
      nodes =
        Array.mapi
          Network.(fun i x -> { x with compute = float_of_int (i + 1) })
          net.nodes
    }
  in
  let it () = Simulator.init protocol net in
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
  let net = Network.T.two_agents ~alpha ~activation_delay:1. in
  let it () =
    let patch = function
      | 0 -> Some attack.Collection.it
      | _ -> None
    in
    Simulator.init ~patch protocol net
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
  let net =
    Network.T.selfish_mining
      ~gamma
      ~activation_delay:1.
      ~propagation_delay:msg_delay
      ~defenders
      ~alpha
  in
  let it () =
    let patch = function
      | 0 -> Some attack.Collection.it
      | _ -> None
    in
    Simulator.init ~patch protocol net
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
