open Cpr_lib

module type Parameters = sig
  (** number of votes (= puzzle solutions) per block *)
  val k : int

  (**
     [`Constant]: 1 per confirmed proof-of-work

     [`Discount]: max k per confirmed block, d/k per confirmed pow solution (d
     ∊ 1..k = height since last block)

     [`Punish]: max k per confirmed block, 1 per pow solution on longest chain
     of votes

     [`Hybrid]: max k per confirmed block, d/k per pow solution on longest
     chain of votes (d ∊ 1..k = height since last block)
  *)
  val incentive_scheme : [ `Constant | `Discount | `Punish | `Hybrid ]

  val subblock_selection : [ `Altruistic | `Heuristic | `Optimal ]
end

module Make (Parameters : Parameters) = struct
  open Parameters

  let key =
    let open Options in
    Format.asprintf "tailstorm-%i-%a-%a" k pp incentive_scheme pp subblock_selection
  ;;

  let description =
    Format.asprintf
      "Tailstorm with k=%i, %a rewards, and %a sub-block selection"
      Parameters.k
      Options.pp
      incentive_scheme
      Options.pp
      subblock_selection
  ;;

  let info =
    let open Info in
    [ string "family" "tailstorm"
    ; int "k" k
    ; Options.info "incentive_scheme" incentive_scheme
    ; Options.info "subblock_selection" subblock_selection
    ]
  ;;

  type data =
    | Summary of { height : int }
    | Vote of
        { height : int
        ; depth : int
        ; miner : int
        }

  let height = function
    | Summary x -> x.height
    | Vote x -> x.height
  ;;

  let depth = function
    | Summary _ -> 0
    | Vote x -> x.depth
  ;;

  let progress x = (height x * k) + depth x |> float_of_int

  let is_vote = function
    | Vote _ -> true
    | _ -> false
  ;;

  let is_summary = function
    | Summary _ -> true
    | _ -> false
  ;;

  let roots = [ Summary { height = 0 } ]

  module Referee (V : GlobalView with type data = data) = struct
    include V

    let info x =
      let open Info in
      match data x with
      | Vote x -> [ string "kind" "vote"; int "height" x.height; int "depth" x.depth ]
      | Summary x -> [ string "kind" "summary"; int "height" x.height ]
    ;;

    let label x =
      match data x with
      | Summary x -> Printf.sprintf "summary %i" x.height
      | Vote x -> Printf.sprintf "vote (%i|%i)" x.height x.depth
    ;;

    let dag_fail (type a) vertices msg : a =
      let meta x = [ label x, "" ] in
      Dag.Exn.raise view meta vertices msg
    ;;

    let is_vote x = is_vote (data x)
    let is_summary x = is_summary (data x)
    let height x = height (data x)
    let depth x = depth (data x)
    let progress x = progress (data x)

    let rec last_summary x =
      if is_summary x
      then x
      else (
        match Dag.parents view x with
        | [ x ] -> last_summary x
        | parents ->
          dag_fail (x :: parents) "last_summary: votes have one parent by dag_validity")
    ;;

    (* smaller is better *)
    let compare_votes_in_block =
      let get x =
        let hash = pow x |> Option.value ~default:(0, 0) in
        depth x, hash
      and ty = Compare.(tuple (neg int) (tuple int int)) in
      Compare.(by ty get)
    ;;

    let votes_only = Dag.filter is_vote view
    let summaries_only = Dag.filter is_summary view

    let validity vertex =
      match data vertex, Dag.parents view vertex with
      | Vote child, [ parent ] ->
        child.depth > 0
        && pow vertex |> Option.is_some
        && child.height = height parent
        && child.depth = depth parent + 1
      | Summary child, (vote0 :: votetl as votes) ->
        let unique_votes () =
          Dag.iterate_ancestors votes_only votes |> Seq.fold_left (fun acc _ -> acc + 1) 0
        and same_summary () =
          (* TODO. I think this check is missing in the other protocols *)
          let parent = last_summary vote0 in
          List.for_all (fun x -> last_summary x $== parent) votetl
        and sorted_votes () =
          Compare.is_sorted ~unique:true compare_votes_in_block votes
        in
        child.height > 0
        && pow vertex = None
        && same_summary ()
        && sorted_votes ()
        && List.for_all is_vote votes
        && unique_votes () = k
        && child.height = height vote0 + 1
      | _ -> false
    ;;

    (** better is bigger *)
    let compare_summaries =
      let open Compare in
      let cmp =
        by int height $ by int (fun x -> List.length (Dag.children votes_only x))
      in
      skip_eq Dag.vertex_eq cmp
    ;;

    let winner l =
      assert (List.for_all is_summary l);
      Compare.first (Compare.neg compare_summaries) 1 l |> Option.get |> List.hd
    ;;

    let precursor this = Dag.parents view this |> fun parents -> List.nth_opt parents 0

    let assign c x =
      match data x with
      | Vote x -> [ x.miner, c ]
      | Summary _ -> []
    ;;

    let reward' ~discount ~punish x =
      let k = float_of_int k in
      let c = (* max reward per vote *) 1. in
      match x.data with
      | Summary _ ->
        (match x.parents with
        | [] -> []
        | hd :: _ as parents ->
          let depth = depth hd in
          let r = if discount then float_of_int depth /. k *. c else c in
          if punish
          then
            Dag.iterate_ancestors votes_only [ hd ]
            |> Seq.map (assign r)
            |> List.of_seq
            |> List.concat
          else
            Dag.iterate_ancestors votes_only parents
            |> Seq.map (assign r)
            |> List.of_seq
            |> List.concat)
      | Vote _x -> []
    ;;

    let reward' =
      match incentive_scheme with
      | `Constant -> reward' ~discount:false ~punish:false
      | `Discount -> reward' ~discount:true ~punish:false
      | `Punish -> reward' ~discount:false ~punish:true
      | `Hybrid -> reward' ~discount:true ~punish:true
    ;;

    let vertex_to_draft b : _ draft_vertex =
      { parents = Dag.parents view b
      ; data = data b
      ; sign = signature b |> Option.is_some
      }
    ;;

    let reward x = vertex_to_draft x |> reward'
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
      | `Withheld | `Released -> true
      | `Received -> false
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
        if n = k
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
            if n' > k || n_fresh < 1
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
               (fun x -> depth x, ((if appended_by_me x then 0 else 1), visible_since x)))
      |> f IntSet.empty 0 []
      |> Option.map
           (List.sort
              Compare.(
                by
                  (tuple (neg int) (tuple int int))
                  (fun x ->
                    let hash = pow x |> Option.value ~default:max_pow in
                    depth x, hash)))
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
      and n = ref k in
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
    let optimal_quorum ~max_options ~vote_filter b =
      assert (is_summary b);
      let reward leaves =
        reward'
          { data = Summary { height = height b + 1 }; parents = leaves; sign = false }
        |> List.fold_left (fun acc (n, x) -> if n = my_id then acc +. x else acc) 0.
      in
      let a =
        Dag.children votes_only b
        |> Dag.iterate_descendants (Dag.filter vote_filter votes_only)
        |> Array.of_seq
      in
      let n = Array.length a in
      if Combinatorics.n_choose_k n k > max_options
      then heuristic_quorum ~vote_filter b
      else if n < k
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
              `Ok (reward leaves, leaves)
          in
          fun c ->
            reset ();
            f c
        in
        let q =
          let l = ref [] in
          let () =
            Combinatorics.iter_n_choose_k (Array.length a) k (fun c ->
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
        Some q)
    ;;

    let quorum =
      match subblock_selection with
      | `Altruistic -> altruistic_quorum
      | `Heuristic -> heuristic_quorum
      | `Optimal -> optimal_quorum ~max_options:100
    ;;

    let puzzle_payload' ~vote_filter b =
      assert (is_summary b);
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
      ; data = Vote { height = height b; depth = depth parent + 1; miner = my_id }
      }
    ;;

    let puzzle_payload = puzzle_payload' ~vote_filter:(fun _ -> true)

    let next_summary' ~vote_filter b =
      quorum ~vote_filter b
      |> Option.map (fun q ->
             { parents = q; data = Summary { height = height b + 1 }; sign = false })
    ;;

    let next_summary = next_summary' ~vote_filter:(fun _ -> true)

    let compare_blocks ~vote_filter =
      let open Compare in
      let count x =
        Dag.iterate_descendants votes_only [ x ]
        |> Seq.filter vote_filter
        |> Seq.fold_left (fun n _ -> n + 1) 0
      and reward x =
        (* disambiguate summaries w/o confirming votes. W/o this, optimal sub-block
           selection does not work *)
        reward x
        |> List.fold_left (fun sum (i, x) -> if i = my_id then sum +. x else sum) 0.
      in
      let cmp = by int height $ by int count (* embed A_k *) $ by float reward in
      skip_eq Dag.vertex_eq cmp
    ;;

    let update_head ?(vote_filter = Fun.const true) ~old consider =
      if compare_blocks ~vote_filter consider old > 0 then consider else old
    ;;

    let handler preferred = function
      | Append x | Network x | ProofOfWork x ->
        if is_summary x
        then update_head ~old:preferred x |> return
        else (
          let s = last_summary x in
          let append =
            match next_summary s with
            | Some block -> [ block ]
            | None -> []
          and share =
            match visibility x with
            | `Withheld when is_vote x -> [ x ]
            | `Withheld ->
              (* TODO. The protocol works w/o this case. However, the Tailstorm_ssz attack
                 space relies on defenders sharing their summaries. It could reconstruct
                 defender's summaries locally, but this is not implemented yet *)
              [ x ]
            | _ -> []
          in
          update_head ~old:preferred s |> return ~append ~share)
    ;;
  end

  let honest (type a) ((module V) : (a, data) local_view) : (a, data) node =
    Node (module Honest (V))
  ;;
end
