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
let ethereum ~incentive_scheme:is =
  let open Ethereum in
  let module M =
    Make (struct
      include Byzantium

      let incentive_scheme = is
    end)
  in
  Protocol (module M)
;;

(** {!nakamoto_ssz} adapted for Ethereum. *)
let ethereum_ssz ~incentive_scheme:is =
  let module M =
    Ethereum_ssz.Make (struct
      include Ethereum.Byzantium

      let incentive_scheme = is
    end)
  in
  AttackSpace (module M)
;;

(** Bₖ as proposed by Keller and Böhme. Parallel Proof-of-Work with Concrete Bounds. 2022.
    {{:https://arxiv.org/abs/2204.00034}Paper.} *)
let bk ~k ~incentive_scheme =
  let module M =
    Bk.Make (struct
      let k = k
      let incentive_scheme = incentive_scheme
    end)
  in
  Protocol (module M)
;;

(** {!nakamoto_ssz} adapted for Bₖ. *)
let bk_ssz ~k ~incentive_scheme =
  let module M =
    Bk_ssz.Make (struct
      let k = k
      let incentive_scheme = incentive_scheme
    end)
  in
  AttackSpace (module M)
;;

(** {!bk} with alternative leader selection mechanism: k - 1 votes, the k-th proof-of-work
    authorizes the next block. *)
let bkll ~k ~incentive_scheme =
  let module M =
    Bkll.Make (struct
      let k = k
      let incentive_scheme = incentive_scheme
    end)
  in
  Protocol (module M)
;;

(** {!nakamoto_ssz} adapted for {!bkll}. *)
let bkll_ssz ~k ~incentive_scheme =
  let module M =
    Bkll_ssz.Make (struct
      let k = k
      let incentive_scheme = incentive_scheme
    end)
  in
  AttackSpace (module M)
;;

(** Tailstorm protocol with k subblocks per (strong) block, subblocks require proof-of-work, strong blocks don't. *)
let tailstorm ~k ~incentive_scheme ~subblock_selection =
  let module M =
    Tailstorm.Make (struct
      let k = k
      let incentive_scheme = incentive_scheme
      let subblock_selection = subblock_selection
    end)
  in
  Protocol (module M)
;;

(** {!nakamoto_ssz} adapted for {!tailstorm}. *)
let tailstorm_ssz ~k ~incentive_scheme ~subblock_selection =
  let module M =
    Tailstorm_ssz.Make (struct
      let k = k
      let incentive_scheme = incentive_scheme
      let subblock_selection = subblock_selection
    end)
  in
  AttackSpace (module M)
;;

(** Modified Tailstorm protocol with k - 1 subblocks per (strong) block, all
 * blocks (including strong ones) require a proof-of-work. I append ll because
 * this protocol is to {!tailstorm}, what {!bkll} is to {!bk}. *)
let tailstormll ~k ~incentive_scheme ~subblock_selection =
  let module M =
    Tailstormll.Make (struct
      let k = k
      let incentive_scheme = incentive_scheme
      let subblock_selection = subblock_selection
    end)
  in
  Protocol (module M)
;;

(** {!nakamoto_ssz} adapted for {!tailstormll}. *)
let tailstormll_ssz ~k ~incentive_scheme ~subblock_selection =
  let module M =
    Tailstormll_ssz.Make (struct
      let k = k
      let incentive_scheme = incentive_scheme
      let subblock_selection = subblock_selection
    end)
  in
  AttackSpace (module M)
;;

(** Almost {!tailstormll} but recovered from June version. *)
let tailstormjune ~k ~incentive_scheme =
  let module M =
    Tailstorm_june.Make (struct
      let k = k
      let incentive_scheme = incentive_scheme
    end)
  in
  Protocol (module M)
;;

(** {!nakamoto_ssz} adapted for {!tailstormjune}. *)
let tailstormjune_ssz ~k ~incentive_scheme =
  let module M =
    Tailstorm_june_ssz.Make (struct
      let k = k
      let incentive_scheme = incentive_scheme
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
      let sim = Simulator.init ~logger (module P) network in
      let target_progress = 1000 in
      Simulator.loop ~activations:target_progress sim;
      let head = Simulator.head sim in
      let (module Ref) = sim.referee in
      let observed_progress = Ref.progress head in
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
      test
        n
        ~activation_delay:16.
        ~orphan_rate_limit:0.1
        (ethereum ~incentive_scheme:`Constant)
    ;;

    let n = "ethereum/hard"

    let%test_unit [%name n] =
      (* ethereum should collect most blocks that would be orphans as uncles *)
      test
        n
        ~activation_delay:1.
        ~orphan_rate_limit:0.2
        (ethereum ~incentive_scheme:`Discount)
    ;;

    let n = "ethereum/real"

    let%test_unit [%name n] =
      (* ethereum should collect most blocks that would be orphans as uncles *)
      test
        n
        ~activation_delay:6.
        ~orphan_rate_limit:0.01
        (ethereum ~incentive_scheme:`Discount)
    ;;

    let n = "ethereum/extreme"

    let%test_unit [%name n] =
      (* fuzzing the uncle inclusion rule *)
      test
        n
        ~activation_delay:0.3
        ~orphan_rate_limit:0.9
        (ethereum ~incentive_scheme:`Discount)
    ;;

    let n = "bk8/easy"

    let%test_unit [%name n] =
      test
        n
        ~activation_delay:10.
        ~orphan_rate_limit:0.1
        (bk ~k:8 ~incentive_scheme:`Constant)
    ;;

    let n = "bk8/hard"

    let%test_unit [%name n] =
      test
        n
        ~activation_delay:1.
        ~orphan_rate_limit:0.3
        (bk ~k:8 ~incentive_scheme:`Block)
    ;;

    let n = "bk32/hard"

    let%test_unit [%name n] =
      test
        n
        ~activation_delay:1.
        ~orphan_rate_limit:0.1
        (bk ~k:32 ~incentive_scheme:`Constant)
    ;;

    let n = "bkll8/easy"

    let%test_unit [%name n] =
      test
        n
        ~activation_delay:10.
        ~orphan_rate_limit:0.1
        (bkll ~k:8 ~incentive_scheme:`Constant)
    ;;

    let n = "bkll8/hard"

    let%test_unit [%name n] =
      test
        n
        ~activation_delay:1.
        ~orphan_rate_limit:0.3
        (bkll ~k:8 ~incentive_scheme:`Constant)
    ;;

    let n = "bkll32/hard"

    let%test_unit [%name n] =
      test
        n
        ~activation_delay:1.
        ~orphan_rate_limit:0.1
        (bkll ~k:32 ~incentive_scheme:`Constant)
    ;;

    let n = "tailstorm8constant/easy"

    let%test_unit [%name n] =
      test
        n
        ~activation_delay:10.
        ~orphan_rate_limit:0.1
        (tailstorm ~subblock_selection:`Optimal ~k:8 ~incentive_scheme:`Constant)
    ;;

    let n = "tailstorm8discount/hard"

    let%test_unit [%name n] =
      test
        n
        ~activation_delay:1.
        ~orphan_rate_limit:0.3
        (tailstorm ~subblock_selection:`Heuristic ~k:8 ~incentive_scheme:`Discount)
    ;;

    let n = "tailstorm32punish/hard"

    let%test_unit [%name n] =
      test
        n
        ~activation_delay:1.
        ~orphan_rate_limit:0.1
        (tailstorm ~subblock_selection:`Altruistic ~k:32 ~incentive_scheme:`Punish)
    ;;

    let n = "tailstormll8constant/easy"

    let%test_unit [%name n] =
      test
        n
        ~activation_delay:10.
        ~orphan_rate_limit:0.1
        (tailstormll ~subblock_selection:`Optimal ~k:8 ~incentive_scheme:`Constant)
    ;;

    let n = "tailstormll8discount/hard"

    let%test_unit [%name n] =
      test
        n
        ~activation_delay:1.
        ~orphan_rate_limit:0.3
        (tailstormll ~subblock_selection:`Optimal ~k:8 ~incentive_scheme:`Discount)
    ;;

    let n = "tailstormll32hybrid/hard"

    let%test_unit [%name n] =
      test
        n
        ~activation_delay:1.
        ~orphan_rate_limit:0.1
        (tailstormll ~subblock_selection:`Altruistic ~k:32 ~incentive_scheme:`Hybrid)
    ;;

    let n = "tailstormjune8constant/easy"

    let%test_unit [%name n] =
      test
        n
        ~activation_delay:10.
        ~orphan_rate_limit:0.1
        (tailstormjune ~k:8 ~incentive_scheme:`Constant)
    ;;

    let n = "tailstormjune8discount/hard"

    let%test_unit [%name n] =
      test
        n
        ~activation_delay:1.
        ~orphan_rate_limit:0.3
        (tailstormjune ~k:8 ~incentive_scheme:`Discount)
    ;;

    let n = "tailstormjune32hybrid/hard"

    let%test_unit [%name n] =
      test
        n
        ~activation_delay:1.
        ~orphan_rate_limit:0.1
        (tailstormjune ~k:32 ~incentive_scheme:`Hybrid)
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
      let sim =
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
      let target_progress = 1000 in
      Simulator.loop ~activations:target_progress sim;
      let head = Simulator.head sim in
      let (module Ref) = sim.referee in
      let observed_progress = Ref.progress head in
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

    let n = "nakamoto/ssz/honest"

    let%test_unit [%name n] = test n ~policy:"honest" ~orphan_rate_limit:0.01 nakamoto_ssz

    let n = "ethereum/ssz/honest"

    let%test_unit [%name n] =
      test
        n
        ~policy:"honest"
        ~orphan_rate_limit:0.01
        (ethereum_ssz ~incentive_scheme:`Constant)
    ;;

    let n = "bk8/ssz/honest"

    let%test_unit [%name n] =
      test
        n
        ~policy:"honest"
        ~orphan_rate_limit:0.01
        (bk_ssz ~k:8 ~incentive_scheme:`Block)
    ;;

    let n = "bkll8/ssz/honest"

    let%test_unit [%name n] =
      test
        n
        ~policy:"honest"
        ~orphan_rate_limit:0.01
        (bkll_ssz ~k:8 ~incentive_scheme:`Constant)
    ;;

    let n = "tailstorm8constant/ssz/honest"

    let%test_unit [%name n] =
      test
        n
        ~policy:"honest"
        ~orphan_rate_limit:0.01
        (tailstorm_ssz ~subblock_selection:`Optimal ~k:8 ~incentive_scheme:`Constant)
    ;;

    let n = "tailstorm8discount/ssz/honest"

    let%test_unit [%name n] =
      test
        n
        ~policy:"honest"
        ~orphan_rate_limit:0.01
        (tailstorm_ssz ~subblock_selection:`Heuristic ~k:8 ~incentive_scheme:`Discount)
    ;;

    let n = "tailstormll8constant/ssz/honest"

    let%test_unit [%name n] =
      test
        n
        ~policy:"honest"
        ~orphan_rate_limit:0.01
        (tailstormll_ssz ~subblock_selection:`Optimal ~k:8 ~incentive_scheme:`Constant)
    ;;

    let n = "tailstormll8discount/ssz/honest"

    let%test_unit [%name n] =
      test
        n
        ~policy:"honest"
        ~orphan_rate_limit:0.01
        (tailstormll_ssz ~subblock_selection:`Heuristic ~k:8 ~incentive_scheme:`Discount)
    ;;

    let n = "tailstormjune8constant/ssz/honest"

    let%test_unit [%name n] =
      test
        n
        ~policy:"honest"
        ~orphan_rate_limit:0.01
        (tailstormjune_ssz ~k:8 ~incentive_scheme:`Constant)
    ;;

    let n = "tailstormjune8discount/ssz/honest"

    let%test_unit [%name n] =
      test
        n
        ~policy:"honest"
        ~orphan_rate_limit:0.01
        (tailstormjune_ssz ~k:8 ~incentive_scheme:`Discount)
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
      let sim =
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
      let target_progress = 1000 in
      Simulator.loop ~activations:target_progress sim;
      let head = Simulator.head sim in
      let (module Ref) = sim.referee in
      let observed_progress = Ref.progress head in
      let observed_orphan_rate =
        let target_progress = float_of_int target_progress in
        (target_progress -. observed_progress) /. target_progress
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

    let%test_unit [%name n] = test n (ethereum_ssz ~incentive_scheme:`Discount)

    let n = "bk8/ssz/random"

    let%test_unit [%name n] = test n (bk_ssz ~k:8 ~incentive_scheme:`Block)

    let n = "bkll8/ssz/random"

    let%test_unit [%name n] = test n (bkll_ssz ~k:8 ~incentive_scheme:`Constant)

    let n = "tailstorm8constant/ssz/random"

    let%test_unit [%name n] =
      test n (tailstorm_ssz ~subblock_selection:`Optimal ~k:8 ~incentive_scheme:`Constant)
    ;;

    let n = "tailstorm8discount/ssz/random"

    let%test_unit [%name n] =
      test
        n
        (tailstorm_ssz ~subblock_selection:`Heuristic ~k:8 ~incentive_scheme:`Discount)
    ;;

    let n = "tailstormll8constant/ssz/random"

    let%test_unit [%name n] =
      test
        n
        (tailstormll_ssz ~subblock_selection:`Optimal ~k:8 ~incentive_scheme:`Constant)
    ;;

    let n = "tailstormll8discount/ssz/random"

    let%test_unit [%name n] =
      test
        n
        (tailstormll_ssz ~subblock_selection:`Heuristic ~k:8 ~incentive_scheme:`Discount)
    ;;

    let n = "tailstormjune8constant/ssz/random"

    let%test_unit [%name n] = test n (tailstormjune_ssz ~k:8 ~incentive_scheme:`Constant)

    let n = "tailstormjune8discount/ssz/random"

    let%test_unit [%name n] = test n (tailstormjune_ssz ~k:8 ~incentive_scheme:`Discount)
  end)
;;

include struct
  module M : sig
    val of_key : string -> (protocol, Rresult.R.msg) result
  end = struct
    open Angstrom

    let whitespace = function
      | ' ' -> true
      | _ -> false
    ;;

    let a_to_z c = 'a' <= c && c <= 'z'
    let digit c = '0' <= c && c <= '9'
    let trim = skip_many (skip whitespace)
    let sep = char '-'

    let int =
      let* str = (sep <|> fail "missing integer option") *> take_while digit in
      match int_of_string_opt str with
      | Some x -> return x
      | None -> fail "could not parse int"
    ;;

    let option choice =
      let* str = (sep <|> fail "missing option") *> take_while a_to_z in
      Options.of_string choice str |> Result.fold ~ok:return ~error:fail
    ;;

    let nakamoto = return nakamoto

    let ethereum =
      let f incentive_scheme = ethereum ~incentive_scheme in
      lift f (option Ethereum.incentive_schemes)
    ;;

    let bk =
      let f k incentive_scheme = bk ~k ~incentive_scheme in
      lift2 f int (option Bk.incentive_schemes)
    ;;

    let bkll =
      let f k incentive_scheme = bkll ~k ~incentive_scheme in
      lift2 f int (option Bkll.incentive_schemes)
    ;;

    let tailstorm =
      let f k incentive_scheme subblock_selection =
        tailstorm ~k ~incentive_scheme ~subblock_selection
      in
      let open Tailstorm in
      lift3 f int (option incentive_schemes) (option subblock_selections)
    ;;

    let tailstormll =
      let f k incentive_scheme subblock_selection =
        tailstormll ~k ~incentive_scheme ~subblock_selection
      in
      let open Tailstormll in
      lift3 f int (option incentive_schemes) (option subblock_selections)
    ;;

    let tailstormjune =
      let f k incentive_scheme = tailstormjune ~k ~incentive_scheme in
      let open Tailstorm_june in
      lift2 f int (option incentive_schemes)
    ;;

    let protocol =
      let* str = take_while a_to_z in
      match str with
      | "nakamoto" -> nakamoto
      | "ethereum" -> ethereum
      | "bk" -> bk
      | "bkll" -> bkll
      | "tailstorm" -> tailstorm
      | "tailstormll" -> tailstormll
      | "tailstormjune" -> tailstormjune
      | _ -> fail "unknown protocol"
    ;;

    let protocol = trim *> protocol <* trim

    let of_key s =
      parse_string ~consume:Consume.All protocol s
      |> Rresult.R.reword_error (fun err ->
             Printf.sprintf "invalid protocol key '%s'%s" s err)
      |> Rresult.R.reword_error Rresult.R.msg
    ;;

    let%expect_test "of_key/to_key" =
      List.iter
        (fun s ->
          let s' =
            match of_key s with
            | Ok (Protocol (module P)) -> P.key
            | Error (`Msg m) -> "Error: " ^ m
          in
          print_endline s')
        [ "nakamoto"
        ; "ethereum-discount"
        ; "bk-2-constant"
        ; "bkll-5-block"
        ; "  tailstorm-42-discount-heuristic  "
        ; " X"
        ; "bk"
        ; "nakamoto 2"
        ];
      [%expect
        {|
      nakamoto
      eth-heaviest_chain-work-2-discount
      bk-2-constant
      bkll-5-block
      tailstorm-42-discount-heuristic
      Error: invalid protocol key ' X': unknown protocol
      Error: invalid protocol key 'bk': missing integer option
      Error: invalid protocol key 'nakamoto 2': end_of_input |}]
    ;;
  end

  include M
end

module Combinatorics = Combinatorics
module Options = Options
