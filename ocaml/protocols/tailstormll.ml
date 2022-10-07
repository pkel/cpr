open Cpr_lib

type reward_scheme =
  | Discount
  | Constant
  | Punish
  | Hybrid
  | Block

let reward_schemes = [ Discount; Constant; Punish; Hybrid; Block ]

let reward_key = function
  | Constant -> "constant"
  | Discount -> "discount"
  | Punish -> "punish"
  | Hybrid -> "hybrid"
  | Block -> "block"
;;

let reward_info = function
  | Constant -> "1 per confirmed puzzle solution"
  | Discount ->
    "max k per confirmed block, d/k per confirmed pow solution (d ∊ 1..k = height since \
     last block)"
  | Punish -> "max k per confirmed block, 1 per pow solution on longest chain of votes"
  | Hybrid ->
    "max k per confirmed block, d/k per pow solution on longest chain of votes (d ∊ 1..k \
     = height since last block)"
  | Block -> "1 per confirmed (strong) block"
;;

type subblock_selection =
  | Altruistic
  | Heuristic
  | Optimal

let subblock_selection_key = function
  | Altruistic -> "altruistic"
  | Heuristic -> "heuristic"
  | Optimal -> "optimal"
;;

module type Parameters = sig
  (** number of votes (= puzzle solutions) per block *)
  val k : int

  val rewards : reward_scheme
  val subblock_selection : subblock_selection
end

module Make (Parameters : Parameters) = struct
  open Parameters

  let key = "tailstormll"

  let info =
    Printf.sprintf
      "Tailstorm/ll with k=%i, '%s' rewards, and '%s' sub block selection"
      Parameters.k
      (reward_key Parameters.rewards)
      (subblock_selection_key Parameters.subblock_selection)
  ;;

  let puzzles_per_block = k

  type data =
    { block : int
    ; vote : int
    }

  let height h = h.block
  let progress x = (x.block * k) + x.vote |> float_of_int
  let is_vote h = h.vote > 0
  let is_block h = h.vote = 0

  let describe h =
    let ty = if is_vote h then "vote" else "block" in
    Printf.sprintf "%s (%i|%i)" ty h.block h.vote
  ;;

  let dag_roots = [ { block = 0; vote = 0 } ]

  module Referee (V : GlobalView with type data = data) = struct
    include V

    let dag_fail (type a) vertices msg : a =
      let meta x = [ describe (data x), "" ] in
      Dag.Exn.raise view meta vertices msg
    ;;

    let is_vote x = is_vote (data x)
    let is_block x = is_block (data x)
    let height x = height (data x)

    let rec last_block x =
      if is_block x
      then x
      else (
        match Dag.parents view x with
        | [ x ] -> last_block x
        | parents ->
          dag_fail
            (x :: parents)
            "last_block: votes have exactly one parent by dag_validity")
    ;;

    (* smaller is better *)
    let compare_votes_in_block =
      let get x =
        let d = data x
        and hash = pow x |> Option.value ~default:(0, 0) in
        d.vote, hash
      and ty = Compare.(tuple (neg int) (tuple int int)) in
      Compare.(by ty get)
    ;;

    let votes_only = Dag.filter is_vote view
    let blocks_only = Dag.filter is_block view

    let dag_validity vertex =
      let child = data vertex in
      child.block >= 0
      && child.vote >= 0
      && child.vote < k
      && pow vertex |> Option.is_some
      &&
      match is_vote vertex, Dag.parents view vertex with
      | true, [ p ] ->
        (* child is vote *)
        let parent = data p in
        child.block = parent.block && child.vote = parent.vote + 1
      | false, p :: votes ->
        (* child is block *)
        let parent = data p in
        let unique_votes =
          Dag.iterate_ancestors votes_only votes |> Seq.fold_left (fun acc _ -> acc + 1) 0
        and sorted_votes = Compare.is_sorted ~unique:true compare_votes_in_block votes in
        is_block p
        && sorted_votes
        && List.for_all is_vote votes
        && unique_votes = k - 1
        && child.block = parent.block + 1
        && child.vote = 0
      | _ -> false
    ;;

    (** better is bigger *)
    let compare_blocks =
      let open Compare in
      let cmp =
        by int height $ by int (fun x -> List.length (Dag.children votes_only x))
      in
      skip_eq Dag.vertex_eq cmp
    ;;

    let winner l =
      assert (List.for_all is_block l);
      Compare.first (Compare.neg compare_blocks) 1 l |> Option.get |> List.hd
    ;;

    let history =
      (* TODO: we could adapt the implementation to not include the last block as first
         parent but to infer it with last_block where needed *)
      Seq.unfold (fun this ->
          List.nth_opt (Dag.parents view this) (if is_block this && k > 1 then 1 else 0)
          |> Option.map (fun next -> this, next))
    ;;

    let constant_block c : _ reward_function =
     fun ~assign x -> if is_block x then assign c x
   ;;

    let reward' ~max_reward_per_block ~discount ~punish : env reward_function =
      let k = float_of_int k in
      let c = max_reward_per_block /. k in
      fun ~assign x ->
        if is_block x
        then (
          match Dag.parents votes_only x with
          | [] -> (* Either genesis or k=1 *) assign c x
          | hd :: _ ->
            let depth = (data hd).vote in
            let r = if discount then (float_of_int depth +. 1.) /. k *. c else c in
            if punish
            then (
              assign r x;
              Dag.iterate_ancestors votes_only [ hd ] |> Seq.iter (assign r))
            else Dag.iterate_ancestors votes_only [ x ] |> Seq.iter (assign r))
    ;;

    let reward =
      let reward = reward' ~max_reward_per_block:(float_of_int k) in
      match Parameters.rewards with
      | Constant -> reward ~discount:false ~punish:false
      | Discount -> reward ~discount:true ~punish:false
      | Punish -> reward ~discount:false ~punish:true
      | Hybrid -> reward ~discount:true ~punish:true
      | Block -> constant_block 1.
    ;;

    (* TODO: add tests for reward functions *)

    let reward_functions =
      let open Collection in
      let x = Parameters.rewards in
      empty |> add ~info:(reward_info x) (reward_key x) reward
    ;;
  end

  let referee (type a) (module V : GlobalView with type env = a and type data = data)
      : (a, data) referee
    =
    (module Referee (V))
  ;;

  module Honest (V : LocalView with type data = data) = struct
    include V
    open Referee (V)

    type state = env Dag.vertex

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
    let altruistic_quorum ~vote_filter b =
      let module IntSet = Set.Make (Int) in
      let rec f ids n q l =
        if n = k - 1
        then Some (List.rev q)
        else (
          match l with
          | [] -> None
          | hd :: tl ->
            let fresh, n_fresh =
              Dag.iterate_ancestors votes_only [ hd ]
              |> Seq.fold_left
                   (fun (fresh, n_fresh) el ->
                     let id = Dag.id el in
                     if IntSet.mem id ids
                     then fresh, n_fresh
                     else IntSet.add id fresh, n_fresh + 1)
                   (IntSet.empty, 0)
            in
            let n' = n + n_fresh in
            if n' > k - 1 || n_fresh < 1
            then (* quorum would grow to big *) f ids n q tl
            else f (IntSet.union fresh ids) n' (hd :: q) tl)
      in
      Dag.iterate_descendants votes_only (Dag.children votes_only b)
      |> Seq.filter vote_filter
      |> List.of_seq
      |> List.sort
           Compare.(
             by
               (tuple (neg int) (tuple int float))
               (fun x ->
                 (data x).vote, ((if appended_by_me x then 0 else 1), visible_since x)))
      |> f IntSet.empty 0 []
      |> Option.map
           (List.sort
              Compare.(
                by
                  (tuple (neg int) (tuple int int))
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
    let heuristic_quorum ~vote_filter b =
      let ht = Hashtbl.create (2 * k)
      and acc = ref []
      and n = ref (k - 1) in
      let included x = Hashtbl.mem ht (Dag.id x) in
      let include_ x =
        assert (not (included x));
        acc := x :: !acc;
        Dag.iterate_ancestors votes_only [ x ]
        |> Seq.iter (fun x ->
               if not (included x)
               then (
                 Hashtbl.replace ht (Dag.id x) true;
                 decr n))
      and reward ?(all = false) x =
        let i = ref 0 in
        let () =
          Dag.iterate_ancestors votes_only [ x ]
          |> Seq.iter (fun x ->
                 if (not (included x)) && (all || appended_by_me x) then incr i)
        in
        !i
      in
      let rec loop () =
        assert (!n >= 0);
        if !n > 0
        then (
          Dag.iterate_descendants votes_only (Dag.children votes_only b)
          |> Seq.filter vote_filter
          |> Seq.filter (fun x -> not (included x))
          |> (* calculate own and overall reward for branch *)
          Seq.map (fun x -> x, reward x, reward ~all:true x)
          |> (* ensure branch fits into quorum *) Seq.filter (fun (_, _, x) -> x <= !n)
          |> List.of_seq
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
          Some (List.sort compare_votes_in_block !acc)
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
    let optimal_quorum ~vote_filter b =
      assert (is_block b);
      if k = 1
      then Some []
      else (
        let a =
          Dag.children votes_only b
          |> Dag.iterate_descendants (Dag.filter vote_filter votes_only)
          |> Array.of_seq
        in
        let n = Array.length a in
        if n < k - 1
        then None
        else (
          let a' =
            let module Map = Map.Make (Int) in
            let _, m =
              Array.fold_left
                (fun (i, m) x -> i + 1, Map.add (Dag.id x) i m)
                (0, Map.empty)
                a
            in
            fun x -> Map.find (Dag.id x) m
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
                     (Dag.parents votes_only a.(hd))
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
                    match rewards with
                    | Block -> Seq.empty
                    | Constant | Discount -> Dag.iterate_ancestors votes_only leaves
                    | Punish | Hybrid ->
                      Dag.iterate_ancestors votes_only [ List.hd leaves ]
                  and per_vote =
                    match rewards with
                    | Block | Constant | Punish -> 1.
                    | Discount | Hybrid ->
                      let depth = (List.hd leaves |> data).vote in
                      float_of_int (depth + 1) /. float_of_int k
                  in
                  Seq.fold_left
                    (fun acc x -> if appended_by_me x then acc +. per_vote else acc)
                    1.
                    rewarded_votes
                in
                `Ok (reward, leaves)
            in
            fun c ->
              reset ();
              f c
          in
          let q =
            let l = ref [] in
            let () =
              Combinatorics.iter_n_choose_k (Array.length a) (k - 1) (fun c ->
                  (* assert (List.length c = k - 1); *)
                  match leaves c with
                  | `Ok x -> l := x :: !l
                  | `Not_connected -> ())
            in
            match !l with
            | hd :: tl ->
              List.fold_left (* find maximum reward *)
                (fun (ar, ac) (br, bc) -> if br > ar then br, bc else ar, ac)
                hd
                tl
              |> snd
            | _ -> failwith "reward_optim_quorum: no choice"
          in
          Some q))
    ;;

    let quorum =
      match subblock_selection with
      | Altruistic -> altruistic_quorum
      | Heuristic -> heuristic_quorum
      | Optimal -> optimal_quorum
    ;;

    let puzzle_payload' ~vote_filter b =
      assert (is_block b);
      match quorum ~vote_filter b with
      | Some q ->
        { parents = b :: q; sign = false; data = { block = height b + 1; vote = 0 } }
      | None ->
        let votes =
          Dag.iterate_descendants (Dag.filter vote_filter votes_only) [ b ]
          |> List.of_seq
          |> List.sort compare_votes_in_block
        in
        let parent =
          match votes with
          | hd :: _ -> hd
          | _ -> b
        in
        { parents = [ parent ]
        ; sign = false
        ; data = { block = height b; vote = (data parent).vote + 1 }
        }
    ;;

    let puzzle_payload = puzzle_payload' ~vote_filter:(fun _ -> true)

    let handler b = function
      | Append _x -> failwith "not implemented"
      | ProofOfWork x | Network x ->
        let share =
          match visibility x with
          | `Withheld -> [ x ]
          | _ -> []
        and pref =
          let c = last_block x in
          let count x =
            Dag.iterate_descendants votes_only [ x ] |> Seq.fold_left (fun n _ -> n + 1) 0
          in
          if height c > height b || (height c = height b && count c > count b)
          then c
          else b
        in
        return ~share pref
    ;;
  end

  let honest (type a) ((module V) : (a, data) local_view) : (a, data) node =
    Node (module Honest (V))
  ;;
end
