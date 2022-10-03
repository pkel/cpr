module Nakamoto = Nakamoto
module Ethereum = Ethereum
module Bk = Bk
module Bkll = Bkll
module Tailstorm = Tailstorm
module Tailstormll = Tailstormll
open Cpr_lib

(** Original proof-of-work consensus as described by Nakamoto. 2008. *)
let nakamoto = Protocol (module Nakamoto)

(** Attack space against {!nakamoto} as described by Sapirshtein, Sompolinsky, and Zohar.
    Optimal Selfish Mining Strategies in Bitcoin. 2016.
    {{:https://arxiv.org/abs/1507.06183}Paper.} *)
let nakamoto_ssz = AttackSpace (module Nakamoto_ssz)

(** Simplified version of GHOST as used in the Ethereum Platform *)
let ethereum =
  let open Ethereum in
  Protocol (module Make (Byzantium))
;;

(** {!nakamoto_ssz} adapted for Ethereum. *)
let ethereum_ssz = AttackSpace (module Ethereum_ssz.Make (Ethereum.Byzantium))

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

(** Tailstorm protocol with k subblocks per (strong) block, subblocks require proof-of-work, strong blocks don't. *)
let tailstorm ~subblock_selection ~k ~rewards =
  let module M =
    Tailstorm.Make (struct
      let k = k
      let rewards = rewards
      let subblock_selection = subblock_selection
    end)
  in
  Protocol (module M)
;;

(** {!nakamoto_ssz} adapted for {!tailstorm}. *)
let tailstorm_ssz ~subblock_selection ~k ~rewards =
  let module M =
    Tailstorm_ssz.Make (struct
      let k = k
      let rewards = rewards
      let subblock_selection = subblock_selection
    end)
  in
  AttackSpace (module M)
;;

(** Modified Tailstorm protocol with k - 1 subblocks per (strong) block, all
 * blocks (including strong ones) require a proof-of-work. I append ll because
 * this protocol is to {!tailstorm}, what {!bkll} is to {!bk}. *)
let tailstormll ~subblock_selection ~k ~rewards =
  let module M =
    Tailstormll.Make (struct
      let k = k
      let rewards = rewards
      let subblock_selection = subblock_selection
    end)
  in
  Protocol (module M)
;;

(** {!nakamoto_ssz} adapted for {!tailstormll}. *)
let tailstormll_ssz ~subblock_selection ~k ~rewards =
  let module M =
    Tailstormll_ssz.Make (struct
      let k = k
      let rewards = rewards
      let subblock_selection = subblock_selection
    end)
  in
  AttackSpace (module M)
;;

(** Deprecated draft attack space against {!tailstormll}. *)
let tailstormll_draft ~subblock_selection ~k ~rewards =
  let module M =
    Tailstormll_draft.Make (struct
      let k = k
      let rewards = rewards
      let subblock_selection = subblock_selection
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
      let target_progress = 1000 in
      Simulator.loop ~activations:(target_progress * P.puzzles_per_block) env;
      let head = Simulator.head env in
      let observed_progress = P.progress (Dag.data head).value in
      let observed_orphan_rate =
        (float_of_int target_progress -. observed_progress)
        /. float_of_int target_progress
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

    let%test_unit "ethereum/easy" =
      (* even nakamoto achieves this! *)
      test ~activation_delay:16. ~orphan_rate_limit:0.1 ethereum
    ;;

    let%test_unit "ethereum/hard" =
      (* ethereum should collect most blocks that would be orphans as uncles *)
      test ~activation_delay:1. ~orphan_rate_limit:0.2 ethereum
    ;;

    let%test_unit "ethereum/real" =
      (* ethereum should collect most blocks that would be orphans as uncles *)
      test ~activation_delay:6. ~orphan_rate_limit:0.01 ethereum
    ;;

    let%test_unit "ethereum/extreme" =
      (* fuzzing the uncle inclusion rule *)
      test ~activation_delay:0.3 ~orphan_rate_limit:0.9 ethereum
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

    let%test_unit "tailstorm8constant/easy" =
      test
        ~activation_delay:10.
        ~orphan_rate_limit:0.1
        (tailstorm ~subblock_selection:Optimal ~k:8 ~rewards:Constant)
    ;;

    let%test_unit "tailstorm8discount/hard" =
      test
        ~activation_delay:1.
        ~orphan_rate_limit:0.3
        (tailstorm ~subblock_selection:Optimal ~k:8 ~rewards:Discount)
    ;;

    let%test_unit "tailstorm32block/hard" =
      test
        ~activation_delay:1.
        ~orphan_rate_limit:0.1
        (tailstorm ~subblock_selection:Altruistic ~k:32 ~rewards:Block)
    ;;

    let%test_unit "tailstormll8constant/easy" =
      test
        ~activation_delay:10.
        ~orphan_rate_limit:0.1
        (tailstormll ~subblock_selection:Optimal ~k:8 ~rewards:Constant)
    ;;

    let%test_unit "tailstormll8discount/hard" =
      test
        ~activation_delay:1.
        ~orphan_rate_limit:0.3
        (tailstormll ~subblock_selection:Optimal ~k:8 ~rewards:Discount)
    ;;

    let%test_unit "tailstormll32block/hard" =
      test
        ~activation_delay:1.
        ~orphan_rate_limit:0.1
        (tailstormll ~subblock_selection:Altruistic ~k:32 ~rewards:Block)
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
      then (
        let all =
          let open Dag in
          iterate_descendants env.global_view (roots env.dag) |> List.of_seq
        in
        Dag.Exn.raise
          env.global_view
          (Simulator.debug_info ~describe:A.Protocol.describe)
          all
          (* failwith *)
          (Printf.sprintf
             "too many orphans: got %.2f, expected %.2f"
             observed_orphan_rate
             orphan_rate_limit))
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

    let%test_unit "tailstorm8constant/ssz/honest" =
      test
        ~policy:"honest"
        ~orphan_rate_limit:0.01
        (tailstorm_ssz ~subblock_selection:Optimal ~k:8 ~rewards:Discount)
    ;;

    let%test_unit "tailstormll8constant/ssz/honest" =
      test
        ~policy:"honest"
        ~orphan_rate_limit:0.01
        (tailstormll_ssz ~subblock_selection:Optimal ~k:8 ~rewards:Constant)
    ;;

    let%test_unit "tailstormll8constant/draft/honest" =
      test
        ~policy:"honest"
        ~orphan_rate_limit:0.01
        (tailstormll_draft ~subblock_selection:Optimal ~k:8 ~rewards:Constant)
    ;;
  end)
;;

let%test_module "random" =
  (module struct
    let test (AttackSpace (module A)) =
      let env =
        let network =
          let propagation_delay = Distributions.exponential ~ev:1. in
          let n_nodes = 3 in
          Network.T.symmetric_clique ~activation_delay:100. ~propagation_delay n_nodes
        in
        let policy _ = Random.int A.Action.n |> A.Action.of_int in
        let random = A.attacker policy in
        Simulator.init
          ~patch:(function
            | 0 -> Some random
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
      if observed_orphan_rate > 0.5
      then
        failwith
          (Printf.sprintf
             "too many orphans: got %.2f, expected %.2f"
             observed_orphan_rate
             0.5)
      else ()
    ;;

    let%test_unit "nakamoto/random" = test nakamoto_ssz
    let%test_unit "bk8/ssz/random" = test (bk_ssz ~k:8)
    let%test_unit "bkll8/ssz/random" = test (bkll_ssz ~k:8)

    let%test_unit "tailstorm8constant/ssz/random" =
      test (tailstorm_ssz ~subblock_selection:Optimal ~k:8 ~rewards:Constant)
    ;;

    let%test_unit "tailstormll8constant/ssz/random" =
      test (tailstormll_ssz ~subblock_selection:Optimal ~k:8 ~rewards:Constant)
    ;;

    let%test_unit "tailstormll8discount/draft/random" =
      test (tailstormll_draft ~subblock_selection:Optimal ~k:8 ~rewards:Discount)
    ;;
  end)
;;
