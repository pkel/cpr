open Cpr_lib

let incentive_schemes = Tailstorm.incentive_schemes
let subblock_selections = Tailstorm.subblock_selections

module type Parameters = Tailstorm.Parameters

module Make (Parameters : Parameters) = struct
  open Parameters

  let key =
    let open Options in
    Format.asprintf "stree-%i-%a-%a" k pp incentive_scheme pp subblock_selection
  ;;

  let description =
    Format.asprintf
      "Simple Parallel PoW with tree-style voting, k=%i, %a rewards, and %a sub-block \
       selection"
      Parameters.k
      Options.pp
      incentive_scheme
      Options.pp
      subblock_selection
  ;;

  let info =
    let open Info in
    [ string "family" "stree"
    ; int "k" k
    ; Options.info "incentive_scheme" incentive_scheme
    ; Options.info "subblock_selection" subblock_selection
    ]
  ;;

  type data =
    { block : int
    ; vote : int
    ; miner : int option
    }

  let height h = h.block
  let depth h = h.vote
  let progress x = (x.block * k) + x.vote |> float_of_int
  let is_vote h = h.vote > 0
  let is_block h = h.vote = 0

  let describe h =
    let ty = if is_vote h then "vote" else "block" in
    Printf.sprintf "%s (%i|%i)" ty h.block h.vote
  ;;

  let roots = [ { block = 0; vote = 0; miner = None } ]

  module Referee (D : BlockDAG with type data = data) = struct
    include D

    let info x =
      let x = data x in
      let open Info in
      if is_vote x
      then [ string "kind" "vote"; int "height" x.block; int "depth" x.vote ]
      else [ string "kind" "summary"; int "height" x.block ]
    ;;

    let label x =
      let x = data x in
      if is_vote x
      then Printf.sprintf "summary %i" x.block
      else Printf.sprintf "vote (%i|%i)" x.block x.vote
    ;;

    let dag_fail (type a) vertices msg : a =
      let meta x = [ Info.string "label" (label x) ] in
      raise_invalid_dag meta vertices msg
    ;;

    let is_vote x = is_vote (data x)
    let is_block x = is_block (data x)
    let height x = height (data x)
    let depth x = depth (data x)
    let progress x = progress (data x)

    let rec last_block x =
      if is_block x
      then x
      else (
        match parents x with
        | [ x ] -> last_block x
        | parents ->
          dag_fail
            (x :: parents)
            "last_block: votes have exactly one parent by dag_validity")
    ;;

    (* smaller is better *)
    let compare_votes_in_block =
      let get x = (data x).vote
      and ty = Compare.(neg int) in
      Compare.(by ty get)
    ;;

    module BlockSet = Set.Make (Block)

    let acc_votes unfold l =
      let open BlockSet in
      let rec f acc stack l =
        match l, stack with
        | [], [] -> acc
        | [], hd :: tl -> f acc tl hd
        | hd :: tl, stack when is_vote hd -> f (add hd acc) (unfold hd :: stack) tl
        | _ :: tl, stack -> f acc stack tl
      in
      f empty [] l
    ;;

    let confirmed_votes x =
      assert (is_block x);
      acc_votes parents (parents x)
    ;;

    let confirming_votes x =
      assert (is_block x);
      acc_votes children (children x)
    ;;

    let validity vertex =
      let child = data vertex in
      child.block >= 0
      && child.vote >= 0
      && child.vote < k
      && has_pow vertex
      && child.miner |> Option.is_some
      &&
      match is_vote vertex, parents vertex with
      | true, [ p ] ->
        (* child is vote *)
        let parent = data p in
        child.block = parent.block && child.vote = parent.vote + 1
      | false, p :: votes ->
        (* child is block *)
        let parent = data p in
        let unique_votes = acc_votes parents votes |> BlockSet.cardinal
        and sorted_votes = Compare.is_sorted compare_votes_in_block votes in
        is_block p
        && sorted_votes
        && unique_votes = k - 1
        && List.for_all (fun x -> is_vote x && Block.eq (last_block x) p) votes
        && child.block = parent.block + 1
        && child.vote = 0
      | _ -> false
    ;;

    (** better is bigger *)
    let compare_blocks =
      let open Compare in
      let cmp =
        by int height $ by int (fun x -> BlockSet.cardinal (confirming_votes x))
      in
      skip_eq Block.eq cmp
    ;;

    let winner l =
      assert (List.for_all is_block l);
      Compare.first (Compare.neg compare_blocks) 1 l |> Option.get |> List.hd
    ;;

    let precursor this = List.nth_opt (parents this) 0

    let assign c x =
      match (data x).miner with
      | Some x -> [ x, c ]
      | None -> []
    ;;

    let reward' ~max_reward_per_block ~discount ~punish x =
      let k = float_of_int k in
      let c = max_reward_per_block /. k in
      if is_block x
      then (
        match parents x |> List.filter is_vote with
        | [] -> []
        | first :: _ as all ->
          let depth = depth first in
          let r = if discount then (float_of_int depth +. 1.) /. k *. c else c in
          let votes =
            if punish
            then BlockSet.add x (acc_votes parents [ first ])
            else BlockSet.add x (acc_votes parents all)
          in
          BlockSet.elements votes |> List.concat_map (assign r))
      else []
    ;;

    let reward =
      let reward = reward' ~max_reward_per_block:(float_of_int k) in
      match incentive_scheme with
      | `Constant -> reward ~discount:false ~punish:false
      | `Discount -> reward ~discount:true ~punish:false
      | `Punish -> reward ~discount:false ~punish:true
      | `Hybrid -> reward ~discount:true ~punish:true
    ;;
  end

  let referee (type a) (module D : BlockDAG with type block = a and type data = data)
      : (a, data) referee
    =
    (module Referee (D))
  ;;

  module Honest (V : View with type data = data) = struct
    include V
    open Referee (V)

    type state = block

    let preferred state = state

    let init ~roots =
      match roots with
      | [ genesis ] -> genesis
      | roots -> dag_fail roots "init: expected single root"
    ;;

    let appended_by_me x =
      match visibility x with
      | `Received -> false
      | `Withheld | `Released -> true
    ;;

    (** Algorithm for selecting sub blocks quickly.

        This version picks the longest branches first. If a branches does not
        fit into the quorum, it is cut down until it fits.

        The chosen sub blocks do not maximize rewards for the miner of the
        block. Thus this implementation does not align with George's
        description of Tailstorm.
    *)
    let altruistic_quorum ~children b =
      let rec f acc n q l =
        if n = k - 1
        then Some (List.rev q)
        else (
          match l with
          | [] -> None
          | hd :: tl ->
            let fresh, n_fresh =
              BlockSet.fold
                (fun el (fresh, n_fresh) ->
                  if BlockSet.mem el acc
                  then fresh, n_fresh
                  else BlockSet.add el fresh, n_fresh + 1)
                (acc_votes parents [ hd ])
                (BlockSet.empty, 0)
            in
            let n' = n + n_fresh in
            if n' > k - 1 || n_fresh < 1
            then (* quorum would grow to big *) f acc n q tl
            else f (BlockSet.union fresh acc) n' (hd :: q) tl)
      in
      acc_votes children (children b)
      |> BlockSet.elements
      |> List.sort
           Compare.(
             by
               (tuple (neg int) (tuple int float))
               (fun x ->
                 (data x).vote, ((if appended_by_me x then 0 else 1), visible_since x)))
      |> f BlockSet.empty 0 []
      |> Option.map
           (List.sort
              Compare.(
                by
                  (tuple (neg int) compare_pow)
                  (fun x ->
                    let d = data x
                    and hash = pow x |> Option.value ~default:max_pow in
                    d.vote, hash)))
    ;;

    (** Algorithm for selecting sub block heuristically but quickly.

        This version tries to select the sub blocks that maximize a miner's
        own rewards. We assume constant reward scheme.

        We first find all branches of depth k-1 or less. We then sort the
        branches by own reward, assuming that the branch will be confirmed in
        the future. We then add the most valuable branch to the quorum.

        If some (= k') sub blocks are missing after the first iteration, we
        search for all remaining branches that would add k' or less sub blocks.
        We sort the branches by the amount of reward they would add. We add the
        most valuable branch an reiterate until enough sub blocks are added.
    *)
    let heuristic_quorum ~children b =
      let leaves = ref BlockSet.empty
      and votes = ref BlockSet.empty
      and n = ref (k - 1) in
      let included x = BlockSet.mem x !votes in
      let include_ x =
        assert (not (included x));
        leaves := BlockSet.add x !leaves;
        acc_votes parents [ x ]
        |> BlockSet.iter (fun x ->
               if not (included x)
               then (
                 votes := BlockSet.add x !votes;
                 decr n))
      and reward ?(all = false) x =
        let i = ref 0 in
        let () =
          acc_votes parents [ x ]
          |> BlockSet.iter (fun x ->
                 if (not (included x)) && (all || appended_by_me x) then incr i)
        in
        !i
      in
      let rec loop () =
        assert (!n >= 0);
        if !n > 0
        then (
          acc_votes children (children b)
          |> BlockSet.elements
          |> List.filter (fun x -> not (included x))
          |> (* calculate own and overall reward for branch *)
          List.map (fun x -> x, reward x, reward ~all:true x)
          |> (* ensure branch fits into quorum *) List.filter (fun (_, _, x) -> x <= !n)
          |> (* prefer own reward, then overall reward *)
          List.sort
            Compare.(
              by int (fun (_, x, _) -> x) |> neg $ (by int (fun (_, _, x) -> x) |> neg))
          |> function
          | [] -> None (* no branches left, not enough votes *)
          | (x, _, _) :: _ ->
            (* add best branch, continue *)
            include_ x;
            loop ())
        else
          (* quorum complete. Ensure that it satisfies quorum validity *)
          Some (BlockSet.elements !leaves |> List.sort compare_votes_in_block)
      in
      loop ()
    ;;

    (** Algorithm for reward-optimizing sub block choice

       Input: last block b

       1. Find all confirming sub blocks of b. Put them into array a.

       2. Check |a| > k. Abort if not.

       3. Prepare mutable list l. Iterate |a| choose k. For each choice, check
       connectivity. Calculate rewards for connected choice; add to l.

       4. Sort l by reward, pick optimal choice q.

       5. Reduce q such that only sub block tree leaves remain.

       6. Sort q by branch depth, descending.

       7. Return q.

       Sorting sub blocks on step (1.) by dag-imposed partial order can simplify
       connectivity check in (3.); e.g., we can iterate the choice in dag-imposed order
       and check if all predecessors have been visited before or equal b.

       We can find the leaves with the same technique. Iterate choice in dag-imposed
       order, for each block mark all its predecessors as redundant. After the iteration,
       leaves are still unmarked.

       It's also easy to select the branch with the highest depth: just pick the last
       entry of q.

       Maybe we can exploit that [iter_n_choose_k] returns the choice in (reverse) sorted
       manner. If we start relying on this, we'd have to add a test for that.

       Calculating rewards will be interesting. We cannot use the reward function of the
       referee, because the block is not yet appended. But maybe we can reuse parts to
       avoid error-prone redundancy. *)
    let optimal_quorum ~max_options ~children b =
      assert (is_block b);
      if k = 1
      then Some []
      else (
        let a = acc_votes children (children b) |> BlockSet.to_seq |> Array.of_seq in
        let n = Array.length a in
        if Combinatorics.n_choose_k n k > max_options
        then heuristic_quorum ~children b
        else if n < k - 1
        then None
        else (
          let a' =
            let module BlockMap = Map.Make (Block) in
            let _, m =
              Array.fold_left
                (fun (i, m) x -> i + 1, BlockMap.add x i m)
                (0, BlockMap.empty)
                a
            in
            fun x -> BlockMap.find x m
          in
          let leaves =
            let reach_buf = Array.make n false in
            let leave_buf = Array.make n true in
            let reset () =
              for i = 0 to n - 1 do
                reach_buf.(i) <- false;
                leave_buf.(i) <- true
              done
            in
            let rec f = function
              | hd :: tl ->
                (* check connectivity, mark non-leaves *)
                if List.for_all
                     (fun p ->
                       leave_buf.(a' p) <- false;
                       reach_buf.(a' p))
                     (parents a.(hd) |> List.filter is_vote)
                then (
                  let () = reach_buf.(hd) <- true in
                  f tl)
                else `Not_connected
              | [] ->
                (* check quorum size (debugging) *)
                (* assert (Array.fold_left (fun c b -> if b then c + 1 else c) 0 reach_buf = k - 1); *)
                let leaves =
                  let l = ref [] in
                  let () =
                    for i = 1 to n do
                      let i' = n - i in
                      if reach_buf.(i') && leave_buf.(i') then l := a.(i') :: !l
                    done
                  in
                  List.sort compare_votes_in_block !l
                in
                let reward =
                  let rewarded_votes =
                    match incentive_scheme with
                    | `Constant | `Discount -> acc_votes parents leaves
                    | `Punish | `Hybrid -> acc_votes parents [ List.hd leaves ]
                  and per_vote =
                    match incentive_scheme with
                    | `Constant | `Punish -> 1.
                    | `Discount | `Hybrid ->
                      let depth = (List.hd leaves |> data).vote in
                      float_of_int (depth + 1) /. float_of_int k
                  in
                  BlockSet.fold
                    (fun x acc -> if appended_by_me x then acc +. per_vote else acc)
                    rewarded_votes
                    1.
                in
                `Ok (reward, leaves)
            in
            fun c ->
              reset ();
              f c
          in
          let q =
            let opt_choice, opt_reward = ref None, ref (-1.) in
            let () =
              Combinatorics.iter_n_choose_k (Array.length a) (k - 1) (fun c ->
                  (* assert (List.length c = k - 1); *)
                  match leaves c with
                  | `Not_connected -> ()
                  | `Ok (reward, choice) ->
                    if reward > !opt_reward
                    then (
                      opt_reward := reward;
                      opt_choice := Some choice))
            in
            match !opt_choice with
            | Some x -> x
            | None -> failwith "reward_optim_quorum: no choice"
          in
          assert (acc_votes parents q |> BlockSet.cardinal = k - 1);
          Some q))
    ;;

    let quorum =
      match subblock_selection with
      | `Altruistic -> altruistic_quorum
      | `Heuristic -> heuristic_quorum
      | `Optimal -> optimal_quorum ~max_options:100
    ;;

    let puzzle_payload' ~vote_filter b =
      let children x = children x |> List.filter vote_filter in
      assert (is_block b);
      match quorum ~children b with
      | Some q ->
        { parents = b :: q
        ; sign = false
        ; data = { block = height b + 1; vote = 0; miner = Some my_id }
        }
      | None ->
        let votes =
          acc_votes children (children b)
          |> BlockSet.elements
          |> List.sort compare_votes_in_block
        in
        let parent =
          match votes with
          | hd :: _ -> hd
          | _ -> b
        in
        { parents = [ parent ]
        ; sign = false
        ; data = { block = height b; vote = (data parent).vote + 1; miner = Some my_id }
        }
    ;;

    let puzzle_payload = puzzle_payload' ~vote_filter:(fun _ -> true)

    let compare_blocks ~vote_filter =
      let children x = children x |> List.filter vote_filter in
      let open Compare in
      let count x = acc_votes children (children x) |> BlockSet.cardinal in
      let cmp =
        by int height
        $ by int count (* embed A_k *)
        $ by (neg float) visible_since (* TODO. Maybe this should be received_at? *)
      in
      skip_eq Block.eq cmp
    ;;

    let update_head ?(vote_filter = Fun.const true) ~old consider =
      assert (is_block consider);
      if compare_blocks ~vote_filter consider old > 0 then consider else old
    ;;

    let handler preferred = function
      | Append _x -> failwith "not implemented"
      | ProofOfWork x | Network x ->
        let b = last_block x in
        let share =
          match visibility x with
          | `Withheld -> [ x ]
          | _ -> []
        in
        update_head ~old:preferred b |> return ~share
    ;;
  end

  let honest (type a) ((module V) : (a, data) view) : (a, data) node =
    Node (module Honest (V))
  ;;
end
