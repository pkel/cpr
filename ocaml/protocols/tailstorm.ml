open Cpr_lib

module type Parameters = sig
  (** number of votes (= puzzle solutions) per block *)
  val k : int
end

module Make (Parameters : Parameters) = struct
  open Parameters

  let key = "tailstorm"
  let info = "Tailstorm with k=" ^ string_of_int k
  let puzzles_per_block = k

  type data =
    { block : int
    ; vote : int
    }

  let height h = h.block
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
      let meta x = [ "describe", describe (data x) ] in
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
        (* votes have only one parent by dag_validity *)
        | [ x ] -> last_block x
        | parents ->
          dag_fail (x :: parents) "last_block: votes have one parent by dag_validity")
    ;;

    let compare_votes_in_block =
      let get x =
        let d = data x
        and hash = pow_hash x |> Option.value ~default:(0, 0) in
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
      && pow_hash vertex |> Option.is_some
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
        by (tuple int int) (fun x ->
            let x = data x in
            x.block, x.vote)
      in
      skip_eq Dag.vertex_eq cmp
    ;;

    let winner l =
      List.map last_block l
      |> Compare.first (Compare.neg compare_blocks) 1
      |> Option.get
      |> List.hd
    ;;

    let constant_block c : _ reward_function =
     fun ~assign x -> if is_block x then assign c x
   ;;

    let reward ~max_reward_per_block ~discount ~punish : env reward_function =
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

    (* TODO: add tests for reward functions *)

    let reward_functions =
      let reward = reward ~max_reward_per_block:(float_of_int k) in
      let open Collection in
      empty
      |> add ~info:"1 per confirmed block" "block" (constant_block 1.)
      |> add
           ~info:
             "max k per confirmed block, d/k per pow solution on longest chain of votes \
              (d ∊ 1..k = height since last block)"
           "hybrid"
           (reward ~discount:true ~punish:true)
      |> add
           ~info:"max k per confirmed block, 1 per pow solution on longest chain of votes"
           "punish"
           (reward ~discount:false ~punish:true)
      |> add
           ~info:
             "max k per confirmed block, d/k per confirmed pow solution (d ∊ 1..k = \
              height since last block)"
           "discount"
           (reward ~discount:true ~punish:false)
      |> add
           ~info:"1 per confirmed pow solution"
           "constant"
           (reward ~discount:false ~punish:false)
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

    let preferred state = last_block state

    let init ~roots =
      match roots with
      | [ genesis ] -> genesis
      | roots -> dag_fail roots "init: expected single root"
    ;;

    module IntSet = Set.Make (struct
      type t = int

      let compare = compare
    end)

    let quorum_old for_block =
      (* look through sub blocks (votes) and assemble quorum if possible. This version
         picks the longest branches first. If a branches does not fit into the quorum, it
         is cut down until it fits.

         The chosen sub blocks are not always optimal for the miner of the block. Thus
         this implementation does not align with George's description of Tailstorm.*)
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
      Dag.iterate_descendants votes_only [ for_block ]
      |> Seq.filter (Dag.vertex_neq for_block)
      |> List.of_seq
      |> List.sort
           Compare.(
             by
               (tuple (neg int) (tuple int float))
               (fun x ->
                 (data x).vote, ((if appended_by_me x then 0 else 1), delivered_at x)))
      |> f IntSet.empty 0 []
      |> Option.map
           (List.sort
              Compare.(
                by
                  (tuple (neg int) (tuple int int))
                  (fun x ->
                    let d = data x
                    and hash = pow_hash x |> Option.value ~default:max_pow_hash in
                    d.vote, hash)))
    ;;

    let quorum for_block =
      (* look through the sub blocks (votes) and assemble a quorum if possible. This
         version tries to select the sub blocks that maximize a miner's own rewards.

         We first find all branches of depth k-1 or less. We then sort the branches by own
         reward, assuming that the branch will be confirmed in the future. We then add the
         most valuable branch to the quorum.

         If some (= k') sub blocks are missing after the first iteration, we search for
         all remaining branches that would add 'k or less sub blocks. We sort the branches
         by the amount of reward they would add. We add the most valuable branch an
         reiterate until enough sub blocks are added.

         TODO. For now, we assume constant reward per proof-of-work. Handling the other
         reward schemes correctly requires a restructuring of the code: reward function
         must become an argument of the protocol. *)
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
          Dag.iterate_descendants votes_only [ for_block ]
          |> Seq.filter (Dag.vertex_neq for_block)
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

    let puzzle_payload preferred =
      let head = last_block preferred in
      match quorum head with
      | Some q ->
        { parents = head :: q
        ; sign = false
        ; data = { block = height head + 1; vote = 0 }
        }
      | None ->
        { parents = [ preferred ]
        ; sign = false
        ; data = { block = height head; vote = (data preferred).vote + 1 }
        }
    ;;

    let handler preferred = function
      | PuzzleSolved v -> { state = v; share = [ v ] }
      | Deliver consider ->
        (* Prefer longest chain of votes after longest chain of blocks *)
        let p = data preferred
        and c = data consider in
        if c.block > p.block || (c.block = p.block && c.vote > p.vote)
        then { state = consider; share = [] }
        else { state = preferred; share = [] }
    ;;
  end

  let honest (type a) ((module V) : (a, data) local_view) : (a, data) node =
    Node (module Honest (V))
  ;;
end
