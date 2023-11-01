open Cpr_lib

let incentive_schemes = [ `Constant; `Discount ]
let subblock_selections = [ `Altruistic; `Heuristic ]

module type Parameters = sig
  (** number of votes (= puzzle solutions) per block *)
  val k : int

  (**
     [`Constant]: 1 per confirmed proof-of-work

     [`Discount]: max k per confirmed block, d/k per confirmed pow solution (d
     ∊ 1..k = height since last block)
  *)
  val incentive_scheme : [ `Constant | `Discount ]

  val subblock_selection : [ `Altruistic | `Heuristic ]
end

module Make (Parameters : Parameters) = struct
  open Parameters

  let () = if k < 2 then failwith "Sdag protocol requires k >= 2"

  let key =
    let open Options in
    Format.asprintf "sdag-%i-%a-%a" k pp incentive_scheme pp subblock_selection
  ;;

  let description =
    Format.asprintf
      "Simple Parallel PoW with DAG-style voting, k=%i, %a rewards, and %a sub-block \
       selection"
      Parameters.k
      Options.pp
      incentive_scheme
      Options.pp
      subblock_selection
  ;;

  let info =
    let open Info in
    [ string "family" "sdag"
    ; int "k" k
    ; Options.info "incentive_scheme" incentive_scheme
    ; Options.info "subblock_selection" subblock_selection
    ]
  ;;

  type data =
    { height : int (* # block ancestors *)
    ; vote : int (* # ancestors of same height *)
    ; miner : int option
    }

  let height x = x.height
  let vote x = x.vote
  let progress x = (x.height * k) + x.vote |> float_of_int
  let is_vote x = x.vote > 0
  let is_block x = x.vote = 0
  let roots = [ { height = 0; vote = 0; miner = None } ]

  module Referee (D : BlockDAG with type data = data) = struct
    include D

    let info x =
      let x = data x in
      let open Info in
      if is_vote x
      then [ string "kind" "vote"; int "height" x.height; int "vote" x.vote ]
      else [ string "kind" "block"; int "height" x.height; int "vote" x.vote ]
    ;;

    let is_vote x = is_vote (data x)
    let is_block x = is_block (data x)
    let height x = height (data x)
    let vote x = vote (data x)
    let progress x = progress (data x)

    let label x =
      let ty = if is_vote x then "vote" else "block" in
      Printf.sprintf "%s (%i|%i)" ty (height x) (vote x)
    ;;

    let dag_fail (type a) vertices msg : a =
      let meta x = [ Info.string "label" (label x) ] in
      raise_invalid_dag meta vertices msg
    ;;

    let rec last_block x =
      if is_block x
      then x
      else (
        match parents x with
        | x :: _ -> last_block x
        | [] -> assert false)
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

    let check label predicate acc =
      if not predicate
      then (
        prerr_string "Sdag.validity: ";
        prerr_endline label);
      acc && predicate
    ;;

    let validity vertex =
      let child = data vertex in
      true
      |> check "height" (child.height >= 0)
      |> check "vote-a" (child.vote >= 0)
      |> check "vote-b" (child.vote <= k)
      |> check "pow" (has_pow vertex)
      |> check "miner" (child.miner |> Option.is_some)
      |> check
           "parents"
           (match parents vertex with
           | p0 :: ps ->
             let pblock = last_block p0 in
             true
             |> check
                  "same block"
                  (List.for_all (fun x -> Block.eq pblock (last_block x)) ps)
             |> check "sorted" (Compare.is_sorted compare_votes_in_block (p0 :: ps))
             |> fun acc ->
             if is_vote vertex
             then
               acc
               |> check "height" (child.height = (data pblock).height)
               |> check
                    "vote"
                    (child.vote = (acc_votes parents [ vertex ] |> BlockSet.cardinal))
             else
               acc
               |> check "cardinal" (confirmed_votes vertex |> BlockSet.cardinal = k - 1)
               |> check "vote" (child.vote = 0)
               |> check "height" (child.height = (data pblock).height + 1)
           | _ -> (* less than one parent *) false)
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
    let assign c x = Option.get (data x).miner, c

    let reward' ~max_reward_per_block ~discount x =
      let k = float_of_int k in
      let c = max_reward_per_block /. k in
      if is_block x
      then (
        let cv = confirmed_votes x in
        let children x = children x |> List.filter (fun x -> BlockSet.mem x cv) in
        let fwd x =
          (* not count x itself, count hypothetical next block *)
          BlockSet.cardinal (acc_votes children [ x ]) - 1 + 1
        and bwd x =
          (* not count x itself *)
          BlockSet.cardinal (acc_votes parents [ x ]) - 1
        in
        let vote_rewards =
          confirmed_votes x
          |> BlockSet.elements
          |> List.map (fun x ->
                 let r =
                   if discount then float_of_int (fwd x + bwd x) /. (k -. 1.) *. c else c
                 in
                 assign r x)
        in
        assign c x :: vote_rewards)
      else []
    ;;

    let reward =
      let reward = reward' ~max_reward_per_block:(float_of_int k) in
      match incentive_scheme with
      | `Constant -> reward ~discount:false
      | `Discount -> reward ~discount:true
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

    (** Algorithm for selecting votes quickly.

        This version picks the high-progress votes first. If the dag does not
        fit into the quorum, it is cut down until it fits.

        The chosen votes do not necessarily maximize rewards for the miner of
        the block.
    *)
    let altruistic_quorum ~children b =
      let rec f acc n l =
        if n = k - 1
        then `Full (BlockSet.elements acc)
        else (
          match l with
          | [] -> `Partial (n, BlockSet.elements acc)
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
            then (* quorum would grow to big *) f acc n tl
            else f (BlockSet.union fresh acc) n' tl)
      in
      acc_votes children (children b)
      |> BlockSet.elements
      |> List.sort
           Compare.(
             by
               (tuple (neg int) (tuple int float))
               (fun x -> vote x, ((if appended_by_me x then 0 else 1), visible_since x)))
      |> f BlockSet.empty 0
    ;;

    (** Algorithm for selecting votes heuristically but quickly.

        This version tries to select the votes that maximize a miner's own
        rewards. We assume constant reward scheme.

        We first find all votes of progress k-1 or less. We then sort the them
        by own reward, assuming that it and all ancestors will be confirmed in
        the future. We then add the most valuable vote to the quorum.

        If some (= k') votes are missing after the first iteration, we search
        for all remaining votes that would add k' or less votes. We sort the
        votes by the amount of reward they would add. We add the most valuable
        vote an reiterate until enough votes are added.
    *)
    let heuristic_quorum ~children b =
      let include_ votes x =
        assert (not (BlockSet.mem x votes));
        BlockSet.fold BlockSet.add votes (acc_votes parents [ x ])
      and reward ?(all = false) votes =
        (* own/all reward if votes would be the final quorum *)
        let children x = children x |> List.filter (fun x -> BlockSet.mem x votes) in
        let fwd x =
          (* not count x itself, count hypothetical next block *)
          BlockSet.cardinal (acc_votes children [ x ]) - 1 + 1
        and bwd x =
          (* not count x itself *)
          BlockSet.cardinal (acc_votes parents [ x ]) - 1
        in
        BlockSet.fold
          (fun x acc -> if all || appended_by_me x then acc + fwd x + bwd x else acc)
          votes
          0
      in
      let votes = ref BlockSet.empty in
      let rec loop () =
        let sn (* size now *) = BlockSet.cardinal !votes in
        if sn < k - 1
        then (
          let mrn (* my reward now *) = reward ~all:false !votes in
          acc_votes children (children b)
          |> BlockSet.elements
          |> List.filter (fun x -> not (BlockSet.mem x !votes))
          |> (* include vote, calculate own reward *)
          List.filter_map (fun x ->
              let votes = include_ !votes x in
              let st (* size then *) = BlockSet.cardinal votes in
              if st <= k - 1
              then (
                let mrt (* my reward then *) = reward ~all:false votes in
                let score =
                  (* reward density *)
                  float_of_int (mrt - mrn) /. float_of_int (st - sn)
                in
                Some (votes, score))
              else (* vote does not fit *)
                None)
          |> (* pick maximum reward density *)
          Compare.(minimum (by (neg float) snd))
          |> (* continue loop *)
          function
          | Some (x, _score) ->
            votes := x;
            loop ()
          | None -> `Partial (sn, BlockSet.elements !votes (* not enough votes *)))
        else `Full (BlockSet.elements !votes)
      in
      loop ()
    ;;

    let quorum =
      match subblock_selection with
      | `Altruistic -> altruistic_quorum
      | `Heuristic -> heuristic_quorum
    ;;

    let puzzle_payload' ~vote_filter b =
      assert (is_block b);
      let children x = children x |> List.filter vote_filter in
      let finalize_quorum votes =
        (* find leaves and ensure quorum validity *)
        let set = BlockSet.of_list votes in
        let is_leave x = not (List.exists (fun c -> BlockSet.mem c set) (children x)) in
        List.filter is_leave votes |> List.sort compare_votes_in_block
      in
      match quorum ~children b with
      | `Full q ->
        (* k-1 sized quorum; build next block *)
        { parents = finalize_quorum q
        ; sign = false
        ; data = { height = height b + 1; vote = 0; miner = Some my_id }
        }
      | `Partial (0, []) ->
        (* no votes yet; build first vote *)
        { parents = [ b ]
        ; sign = false
        ; data = { height = height b; vote = 1; miner = Some my_id }
        }
      | `Partial (0, _) -> failwith "invalid quorum function"
      | `Partial (n, votes) ->
        (* n votes; build another vote *)
        { parents = finalize_quorum votes
        ; sign = false
        ; data = { height = height b; vote = n + 1; miner = Some my_id }
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
