open Cpr_lib

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

(** {!bk} with alternative leader selection mechanism: k - 1 votes, the k-th proof-of-work
    authorizes the next block. *)
let bkll ~k =
  let module M =
    Bkll.Make (struct
      let k = k
    end)
  in
  Protocol (module M)
;;

(** {!nakamoto_ssz} adapted for {!bkll}. *)
let bkll_ssz ~k =
  let module M =
    Bkll_ssz.Make (struct
      let k = k
    end)
  in
  AttackSpace (module M)
;;

(** Tailstorm protocol with k - 1 subblocks per (strong) block *)
let tailstorm ~k =
  let module M =
    Tailstorm.Make (struct
      let k = k
    end)
  in
  Protocol (module M)
;;

(** {!nakamoto_ssz} adapted for {!tailstorm}. *)
let tailstorm_ssz ~k =
  let module M =
    Tailstorm_ssz.Make (struct
      let k = k
    end)
  in
  AttackSpace (module M)
;;

(** Deprecated draft attack space against {!tailstorm}. *)
let tailstorm_draft ~k =
  let module M =
    Tailstorm_draft.Make (struct
      let k = k
    end)
  in
  AttackSpace (module M)
;;

let%test_module "protocol" =
  (module struct
    let test ~activation_delay ~orphan_rate_limit (Protocol (module P)) =
      let env =
        let network =
          let propagation_delay = Distributions.exponential ~ev:1. in
          let n_nodes = 7 in
          Network.T.symmetric_clique ~activation_delay ~propagation_delay n_nodes
        in
        Simulator.init (module P) network
      in
      let target_height = 1000 in
      Simulator.loop ~activations:(target_height * P.puzzles_per_block) env;
      let head = Simulator.head env in
      let observed_height = P.height (Dag.data head).value in
      let observed_orphan_rate =
        float_of_int (target_height - observed_height) /. float_of_int target_height
      in
      if observed_orphan_rate > orphan_rate_limit
      then
        failwith
          (Printf.sprintf
             "too many orphans: got %.2f, expected %.2f"
             observed_orphan_rate
             orphan_rate_limit)
      else ()
    ;;

    let%test_unit "nakamoto/easy" =
      test ~activation_delay:16. ~orphan_rate_limit:0.1 nakamoto
    ;;

    let%test_unit "nakamoto/hard" =
      test ~activation_delay:1. ~orphan_rate_limit:0.5 nakamoto
    ;;

    let%test_unit "bk8/easy" = test ~activation_delay:10. ~orphan_rate_limit:0.1 (bk ~k:8)
    let%test_unit "bk8/hard" = test ~activation_delay:1. ~orphan_rate_limit:0.3 (bk ~k:8)

    let%test_unit "bk32/hard" =
      test ~activation_delay:1. ~orphan_rate_limit:0.1 (bk ~k:32)
    ;;

    let%test_unit "bkll8/easy" =
      test ~activation_delay:10. ~orphan_rate_limit:0.1 (bkll ~k:8)
    ;;

    let%test_unit "bkll8/hard" =
      test ~activation_delay:1. ~orphan_rate_limit:0.3 (bkll ~k:8)
    ;;

    let%test_unit "bkll32/hard" =
      test ~activation_delay:1. ~orphan_rate_limit:0.1 (bkll ~k:32)
    ;;

    let%test_unit "tailstorm8/easy" =
      test ~activation_delay:10. ~orphan_rate_limit:0.1 (tailstorm ~k:8)
    ;;

    let%test_unit "tailstorm8/hard" =
      test ~activation_delay:1. ~orphan_rate_limit:0.3 (tailstorm ~k:8)
    ;;

    let%test_unit "tailstorm32/hard" =
      test ~activation_delay:1. ~orphan_rate_limit:0.1 (tailstorm ~k:32)
    ;;
  end)
;;

let%test_module "policy" =
  (module struct
    let test ~policy ~orphan_rate_limit (AttackSpace (module A)) =
      let env =
        let network =
          let propagation_delay = Distributions.exponential ~ev:1. in
          let n_nodes = 3 in
          Network.T.symmetric_clique ~activation_delay:100. ~propagation_delay n_nodes
        in
        let honest =
          let policy = Collection.get policy A.policies |> Option.get in
          A.attacker policy.it
        in
        Simulator.init
          ~patch:(function
            | 0 -> Some honest
            | _ -> None)
          (module A.Protocol)
          network
      in
      let target_height = 1000 in
      Simulator.loop ~activations:(target_height * A.Protocol.puzzles_per_block) env;
      let head = Simulator.head env in
      let observed_height = A.Protocol.height (Dag.data head).value in
      let observed_orphan_rate =
        float_of_int (target_height - observed_height) /. float_of_int target_height
      in
      if observed_orphan_rate > orphan_rate_limit
      then
        failwith
          (Printf.sprintf
             "too many orphans: got %.2f, expected %.2f"
             observed_orphan_rate
             orphan_rate_limit)
      else ()
    ;;

    let%test_unit "nakamoto/ssz/honest" =
      test ~policy:"honest" ~orphan_rate_limit:0.01 nakamoto_ssz
    ;;

    let%test_unit "bk8/ssz/honest" =
      test ~policy:"honest" ~orphan_rate_limit:0.01 (bk_ssz ~k:8)
    ;;

    let%test_unit "bkll8/ssz/honest" =
      test ~policy:"honest" ~orphan_rate_limit:0.01 (bkll_ssz ~k:8)
    ;;

    let%test_unit "tailstorm8/ssz/honest" =
      test ~policy:"honest" ~orphan_rate_limit:0.01 (tailstorm_ssz ~k:8)
    ;;

    let%test_unit "tailstorm8/draft/honest" =
      test ~policy:"honest" ~orphan_rate_limit:0.01 (tailstorm_draft ~k:8)
    ;;
  end)
;;
