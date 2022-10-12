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
    let test name ~activation_delay ~orphan_rate_limit (Protocol (module P)) =
      let network =
        let propagation_delay = Distributions.exponential ~ev:1. in
        let n_nodes = 7 in
        Network.T.symmetric_clique ~activation_delay ~propagation_delay n_nodes
      in
      let log, logger = Log.GraphLogger.create network in
      let env = Simulator.init ~logger (module P) network in
      let target_progress = 1000 in
      Simulator.loop ~activations:target_progress env;
      let head = Simulator.head env in
      let observed_progress = P.progress (Dag.data head).value in
      let observed_orphan_rate =
        let target_progress = float_of_int target_progress in
        (target_progress -. observed_progress) /. target_progress
      in
      if observed_orphan_rate > orphan_rate_limit
      then (
        let g = Log.GraphLogger.to_graphml log in
        let file =
          "failed_"
          ^ String.map
              (function
                | '/' -> '_'
                | x -> x)
              name
          |> Fpath.of_string
          |> Rresult.R.failwith_error_msg
          |> Fpath.set_ext "graphml"
        in
        GraphML.write_graph g file |> Rresult.R.failwith_error_msg;
        Format.kasprintf
          failwith
          "too many orphans: got %.2f, expected %.2f; see %a"
          observed_orphan_rate
          orphan_rate_limit
          Fpath.pp
          file)
      else ()
    ;;

    let n = "nakamoto/easy"

    let%test_unit [%name n] = test n ~activation_delay:16. ~orphan_rate_limit:0.1 nakamoto

    let n = "nakamoto/hard"

    let%test_unit [%name n] = test n ~activation_delay:1. ~orphan_rate_limit:0.5 nakamoto

    let n = "ethereum/easy"

    let%test_unit [%name n] =
      (* even nakamoto achieves this! *)
      test n ~activation_delay:16. ~orphan_rate_limit:0.1 ethereum
    ;;

    let n = "ethereum/hard"

    let%test_unit [%name n] =
      (* ethereum should collect most blocks that would be orphans as uncles *)
      test n ~activation_delay:1. ~orphan_rate_limit:0.2 ethereum
    ;;

    let n = "ethereum/real"

    let%test_unit [%name n] =
      (* ethereum should collect most blocks that would be orphans as uncles *)
      test n ~activation_delay:6. ~orphan_rate_limit:0.01 ethereum
    ;;

    let n = "ethereum/extreme"

    let%test_unit [%name n] =
      (* fuzzing the uncle inclusion rule *)
      test n ~activation_delay:0.3 ~orphan_rate_limit:0.9 ethereum
    ;;

    let n = "bk8/easy"

    let%test_unit [%name n] =
      test n ~activation_delay:10. ~orphan_rate_limit:0.1 (bk ~k:8)
    ;;

    let n = "bk8/hard"

    let%test_unit [%name n] = test n ~activation_delay:1. ~orphan_rate_limit:0.3 (bk ~k:8)

    let n = "bk32/hard"

    let%test_unit [%name n] =
      test n ~activation_delay:1. ~orphan_rate_limit:0.1 (bk ~k:32)
    ;;

    let n = "bkll8/easy"

    let%test_unit [%name n] =
      test n ~activation_delay:10. ~orphan_rate_limit:0.1 (bkll ~k:8)
    ;;

    let n = "bkll8/hard"

    let%test_unit [%name n] =
      test n ~activation_delay:1. ~orphan_rate_limit:0.3 (bkll ~k:8)
    ;;

    let n = "bkll32/hard"

    let%test_unit [%name n] =
      test n ~activation_delay:1. ~orphan_rate_limit:0.1 (bkll ~k:32)
    ;;

    let n = "tailstorm8constant/easy"

    let%test_unit [%name n] =
      test
        n
        ~activation_delay:10.
        ~orphan_rate_limit:0.1
        (tailstorm ~subblock_selection:Optimal ~k:8 ~rewards:Constant)
    ;;

    let n = "tailstorm8discount/hard"

    let%test_unit [%name n] =
      test
        n
        ~activation_delay:1.
        ~orphan_rate_limit:0.3
        (tailstorm ~subblock_selection:Heuristic ~k:8 ~rewards:Discount)
    ;;

    let n = "tailstorm32punish/hard"

    let%test_unit [%name n] =
      test
        n
        ~activation_delay:1.
        ~orphan_rate_limit:0.1
        (tailstorm ~subblock_selection:Altruistic ~k:32 ~rewards:Punish)
    ;;

    let n = "tailstormll8constant/easy"

    let%test_unit [%name n] =
      test
        n
        ~activation_delay:10.
        ~orphan_rate_limit:0.1
        (tailstormll ~subblock_selection:Optimal ~k:8 ~rewards:Constant)
    ;;

    let n = "tailstormll8discount/hard"

    let%test_unit [%name n] =
      test
        n
        ~activation_delay:1.
        ~orphan_rate_limit:0.3
        (tailstormll ~subblock_selection:Optimal ~k:8 ~rewards:Discount)
    ;;

    let n = "tailstormll32block/hard"

    let%test_unit [%name n] =
      test
        n
        ~activation_delay:1.
        ~orphan_rate_limit:0.1
        (tailstormll ~subblock_selection:Altruistic ~k:32 ~rewards:Block)
    ;;
  end)
;;

let%test_module "policy" =
  (module struct
    let test name ~policy ~orphan_rate_limit (AttackSpace (module A)) =
      let network =
        let propagation_delay = Distributions.exponential ~ev:1. in
        let n_nodes = 3 in
        Network.T.symmetric_clique ~activation_delay:100. ~propagation_delay n_nodes
      in
      let log, logger = Log.GraphLogger.create network in
      let env =
        let honest =
          let policy = Collection.get policy A.policies |> Option.get in
          A.attacker policy.it
        in
        Simulator.init
          ~logger
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
        let g = Log.GraphLogger.to_graphml log in
        let file =
          "failed_"
          ^ String.map
              (function
                | '/' -> '_'
                | x -> x)
              name
          |> Fpath.of_string
          |> Rresult.R.failwith_error_msg
          |> Fpath.set_ext "graphml"
        in
        GraphML.write_graph g file |> Rresult.R.failwith_error_msg;
        Format.kasprintf
          failwith
          "too many orphans: got %.2f, expected %.2f; see %a"
          observed_orphan_rate
          orphan_rate_limit
          Fpath.pp
          file)
      else ()
    ;;

    let n = "nakamoto/ssz/honest"

    let%test_unit [%name n] = test n ~policy:"honest" ~orphan_rate_limit:0.01 nakamoto_ssz

    let n = "ethereum/ssz/honest"

    let%test_unit [%name n] = test n ~policy:"honest" ~orphan_rate_limit:0.01 ethereum_ssz

    let n = "bk8/ssz/honest"

    let%test_unit [%name n] =
      test n ~policy:"honest" ~orphan_rate_limit:0.01 (bk_ssz ~k:8)
    ;;

    let n = "bkll8/ssz/honest"

    let%test_unit [%name n] =
      test n ~policy:"honest" ~orphan_rate_limit:0.01 (bkll_ssz ~k:8)
    ;;

    let n = "tailstorm8constant/ssz/honest"

    let%test_unit [%name n] =
      test
        n
        ~policy:"honest"
        ~orphan_rate_limit:0.01
        (tailstorm_ssz ~subblock_selection:Optimal ~k:8 ~rewards:Constant)
    ;;

    let n = "tailstorm8discount/ssz/honest"

    let%test_unit [%name n] =
      test
        n
        ~policy:"honest"
        ~orphan_rate_limit:0.01
        (tailstorm_ssz ~subblock_selection:Heuristic ~k:8 ~rewards:Discount)
    ;;

    let n = "tailstormll8constant/ssz/honest"

    let%test_unit [%name n] =
      test
        n
        ~policy:"honest"
        ~orphan_rate_limit:0.01
        (tailstormll_ssz ~subblock_selection:Optimal ~k:8 ~rewards:Constant)
    ;;

    let n = "tailstormll8discount/ssz/honest"

    let%test_unit [%name n] =
      test
        n
        ~policy:"honest"
        ~orphan_rate_limit:0.01
        (tailstormll_ssz ~subblock_selection:Heuristic ~k:8 ~rewards:Discount)
    ;;

    let n = "tailstormll8constant/draft/honest"

    let%test_unit [%name n] =
      test
        n
        ~policy:"honest"
        ~orphan_rate_limit:0.01
        (tailstormll_draft ~subblock_selection:Optimal ~k:8 ~rewards:Constant)
    ;;
  end)
;;

let%test_module "random" =
  (module struct
    let test name (AttackSpace (module A)) =
      let network =
        let propagation_delay = Distributions.exponential ~ev:1. in
        let n_nodes = 3 in
        Network.T.symmetric_clique ~activation_delay:100. ~propagation_delay n_nodes
      in
      let log, logger = Log.GraphLogger.create network in
      let env =
        let policy _ = Random.int A.Action.n |> A.Action.of_int in
        let random = A.attacker policy in
        Simulator.init
          ~logger
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
      then (
        let g = Log.GraphLogger.to_graphml log in
        let file =
          "failed_"
          ^ String.map
              (function
                | '/' -> '_'
                | x -> x)
              name
          |> Fpath.of_string
          |> Rresult.R.failwith_error_msg
          |> Fpath.set_ext "graphml"
        in
        GraphML.write_graph g file |> Rresult.R.failwith_error_msg;
        Format.kasprintf
          failwith
          "too many orphans: got %.2f, expected %.2f; see %a"
          observed_orphan_rate
          0.5
          Fpath.pp
          file)
      else ()
    ;;

    let n = "nakamoto/random"

    let%test_unit [%name n] = test n nakamoto_ssz

    let n = "ethereum/random"

    let%test_unit [%name n] = test n ethereum_ssz

    let n = "bk8/ssz/random"

    let%test_unit [%name n] = test n (bk_ssz ~k:8)

    let n = "bkll8/ssz/random"

    let%test_unit [%name n] = test n (bkll_ssz ~k:8)

    let n = "tailstorm8constant/ssz/random"

    let%test_unit [%name n] =
      test n (tailstorm_ssz ~subblock_selection:Optimal ~k:8 ~rewards:Constant)
    ;;

    let n = "tailstorm8discount/ssz/random"

    let%test_unit [%name n] =
      test n (tailstorm_ssz ~subblock_selection:Heuristic ~k:8 ~rewards:Discount)
    ;;

    let n = "tailstormll8constant/ssz/random"

    let%test_unit [%name n] =
      test n (tailstormll_ssz ~subblock_selection:Optimal ~k:8 ~rewards:Constant)
    ;;

    let n = "tailstormll8discount/draft/random"

    let%test_unit [%name n] =
      test n (tailstormll_draft ~subblock_selection:Heuristic ~k:8 ~rewards:Discount)
    ;;
  end)
;;
