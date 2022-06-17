open Cpr_lib.Next

(** Original proof-of-work consensus as described by Nakamoto. 2008. *)
let nakamoto = Protocol (module Nakamoto)

(** Attack space against {!nakamoto} as described by Sapirshtein, Sompolinsky, and Zohar.
    Optimal Selfish Mining Strategies in Bitcoin. 2016.
    {{:https://arxiv.org/abs/1507.06183}Paper.} *)
let nakamoto_ssz = AttackSpace (module Nakamoto_ssz)

(** Bₖ as proposed by Keller and Böhme. Parallel Proof-of-Work with Concrete Bounds. 2022.
    {{:https://arxiv.org/abs/2204.00034}Paper.} *)
let bk ~k =
  let module M =
    Bk.Make (struct
      let k = k
    end)
  in
  Protocol (module M)
;;

(** {!nakamoto_ssz} adapted for Bₖ. *)
let bk_ssz ~k =
  let module M =
    Bk_ssz.Make (struct
      let k = k
    end)
  in
  AttackSpace (module M)
;;

(** deprecated / old interface *)

module B_k_lessleader = B_k_lessleader
module Tailstorm = Tailstorm
