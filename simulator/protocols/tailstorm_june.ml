open Cpr_lib

(* This is an attempt to map Tailstorm(ll) as we had in in June, i.e., successful WandB
   run 257 and git commit cc21ff0f3f, to the updated simulator infrastructure.

   After satisfying the new simulator's requirements, the diff with tailstormll.ml is
   rather small. It's probably wise to drop this implementation here to avoid redundancy.
   But let's first reproduce WandB run 257 with this one. *)

let incentive_schemes = [ `Block; `Constant; `Discount; `Hybrid; `Punish ]

module type Parameters = sig
  (** number of votes (= puzzle solutions) per block *)
  val k : int

  val incentive_scheme : [ `Block | `Constant | `Discount | `Hybrid | `Punish ]
end

module Make (Parameters : Parameters) = struct
  open Parameters

  let key =
    let open Options in
    Format.asprintf "tailstormjune-%i-%a" k pp incentive_scheme
  ;;

  let description =
    Format.asprintf
      "Tailstorm/ll (June '22 version) with k=%i and %a rewards"
      Parameters.k
      Options.pp
      incentive_scheme
  ;;

  let info =
    let open Info in
    [ string "family" "tailstormjune"
    ; int "k" k
    ; Options.info "incentive_scheme" incentive_scheme
    ]
  ;;

  type data =
    { block : int
    ; vote : int
    ; miner : int option
    }

  let height h = h.block
  let is_vote h = h.vote > 0
  let is_block h = h.vote = 0
  let progress x = (x.block * k) + x.vote |> float_of_int

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
    let progress x = progress (data x)

    let rec last_block x =
      if is_block x
      then x
      else (
        match parents x with
        | [ x ] -> last_block x
        | parents ->
          dag_fail (x :: parents) "last_block: votes have one parent by dag_validity")
    ;;

    (* smaller is better *)
    let compare_votes_in_block =
      let get x =
        let d = data x
        and hash = pow x |> Option.value ~default:min_pow in
        d.vote, hash
      and ty = Compare.(tuple (neg int) compare_pow) in
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

    let validity vertex =
      let child = data vertex in
      child.block >= 0
      && child.vote >= 0
      && child.vote < k
      && pow vertex |> Option.is_some
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
      skip_eq Block.eq cmp
    ;;

    let winner l =
      List.map last_block l
      |> Compare.first (Compare.neg compare_blocks) 1
      |> Option.get
      |> List.hd
    ;;

    let precursor this = List.nth_opt (parents this) 0

    let assign c x =
      match (data x).miner with
      | Some x -> [ x, c ]
      | None -> []
    ;;

    let constant_block c x = if is_block x then assign c x else []

    let reward' ~max_reward_per_block ~discount ~punish x =
      let k = float_of_int k in
      let c = max_reward_per_block /. k in
      if is_block x
      then (
        match parents x |> List.filter is_vote with
        | [] -> (* Either genesis or k=1 *) []
        | first :: _ as all ->
          let depth = (data first).vote in
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
      | `Block -> constant_block (float_of_int k)
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

    let preferred state = last_block state

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

    let quorum_old for_block =
      (* look through sub blocks (votes) and assemble quorum if possible. This version
         picks the longest branches first. If a branches does not fit into the quorum, it
         is cut down until it fits.

         The chosen sub blocks are not always optimal for the miner of the block. Thus
         this implementation does not align with George's description of Tailstorm.*)
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
      acc_votes children (children for_block)
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
      let included x =
        Hashtbl.mem ht x
        (* tailstormll.ml has a version w/o hashtbl. Should be preferred because this
           hashes blocks including parents and children recursively *)
      in
      let include_ x =
        assert (not (included x));
        acc := x :: !acc;
        acc_votes parents [ x ]
        |> BlockSet.iter (fun x ->
               if not (included x)
               then (
                 Hashtbl.replace ht x true;
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
          acc_votes children (children for_block)
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
          Some (List.sort compare_votes_in_block !acc)
      in
      loop ()
    ;;

    let puzzle_payload preferred =
      let block = last_block preferred in
      match quorum block with
      | Some q ->
        { parents = block :: q
        ; sign = false
        ; data = { block = height block + 1; vote = 0; miner = Some my_id }
        }
      | None ->
        let votes =
          acc_votes children (children block)
          |> BlockSet.elements
          |> List.sort compare_votes_in_block
        in
        let parent =
          match votes with
          | hd :: _ -> hd
          | _ -> block
        in
        { parents = [ parent ]
        ; sign = false
        ; data =
            { block = height block; vote = (data parent).vote + 1; miner = Some my_id }
        }
    ;;

    let handler preferred = function
      | Append _x -> failwith "not implemented"
      | ProofOfWork v -> { state = v; share = [ v ]; append = [] }
      | Network consider ->
        (* Prefer longest chain of votes after longest chain of blocks *)
        let p = data preferred
        and c = data consider in
        if c.block > p.block || (c.block = p.block && c.vote > p.vote)
        then { state = consider; share = []; append = [] }
        else { state = preferred; share = []; append = [] }
    ;;
  end

  let honest (type a) ((module V) : (a, data) view) : (a, data) node =
    Node (module Honest (V))
  ;;
end
