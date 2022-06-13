open Cpr_lib

type dag_data =
  { block : int
  ; vote : int
  }

let is_vote h = h.vote > 0
let is_block h = h.vote = 0
let height h = h.block

let describe h =
  let ty = if is_vote h then "vote" else "block" in
  Printf.sprintf "%s (%i|%i)" ty h.block h.vote
;;

let cmp_votes_in_block ~data ~pow_hash =
  let get x =
    let d = data x
    and hash = pow_hash x |> Option.value ~default:(0, 0) in
    d.vote, hash
  and ty = Compare.(tuple (neg int) (tuple int int)) in
  Compare.(by ty get)
;;

let dag_validity ~k (v : _ global_view) n =
  let child = v.data n in
  child.block >= 0
  && child.vote >= 0
  && child.vote < k
  && v.pow_hash n |> Option.is_some
  &&
  match v.data n |> is_vote, Dag.parents v.view n with
  | true, [ p ] ->
    (* child is vote *)
    let parent = v.data p in
    child.block = parent.block && child.vote = parent.vote + 1
  | false, p :: votes ->
    (* child is block *)
    let parent = v.data p in
    let unique_votes =
      let votes_only = Dag.filter (fun n -> v.data n |> is_vote) v.view in
      Dag.iterate_ancestors votes_only votes |> Seq.fold_left (fun acc _ -> acc + 1) 0
    and sorted_votes =
      is_sorted ~unique:true (cmp_votes_in_block ~data:v.data ~pow_hash:v.pow_hash) votes
    in
    is_block parent
    && sorted_votes
    && List.for_all (fun n -> v.data n |> is_vote) votes
    && unique_votes = k - 1
    && child.block = parent.block + 1
    && child.vote = 0
  | _ -> false
;;

let dag_roots = [ { block = 0; vote = 0 } ]

let init ~roots =
  match roots with
  | [ genesis ] -> genesis
  | _ -> failwith "invalid roots"
;;

type ('env, 'dag_data) extended_view =
  { view : 'env Dag.view
  ; data : 'env Dag.vertex -> 'dag_data
  ; votes_only : 'env Dag.view
  ; blocks_only : 'env Dag.view
  ; delivered_at : 'env Dag.vertex -> float
  ; pow_hash : 'env Dag.vertex -> (int * int) option
  ; appended_by_me : 'env Dag.vertex -> bool
  ; released : 'env Dag.vertex -> bool
  ; my_id : int
  }

let max_pow_hash = max_int, max_int

let extend_view (x : _ local_view) =
  { view = x.view
  ; data = x.data
  ; votes_only = Dag.filter (fun n -> x.data n |> is_vote) x.view
  ; blocks_only = Dag.filter (fun n -> x.data n |> is_block) x.view
  ; delivered_at = x.delivered_at
  ; pow_hash = x.pow_hash
  ; appended_by_me = x.appended_by_me
  ; released = x.released
  ; my_id = x.my_id
  }
;;

let rec last_block v n =
  if v.data n |> is_block
  then n
  else (
    match Dag.parents v.view n with
    | [ gnode ] -> last_block v gnode
    | _ -> failwith "invalid dag" (* votes have only one parent by dag_validity *))
;;

module IntSet = Set.Make (struct
  type t = int

  let compare = compare
end)

let quorum_old ~k v for_block =
  (* look through sub blocks (votes) and assemble quorum if possible. This version picks
     the longest branches first. If a branches does not fit into the quorum, it is cut
     down until it fits.

     The chosen sub blocks are not always optimal for the miner of the block. Thus this
     implementation does not align with George's description of Tailstorm.*)
  let rec f ids n q l =
    if n = k - 1
    then Some (List.rev q)
    else (
      match l with
      | [] -> None
      | hd :: tl ->
        let fresh, n_fresh =
          Dag.iterate_ancestors v.votes_only [ hd ]
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
  Dag.iterate_descendants v.votes_only [ for_block ]
  |> Seq.filter (Dag.vertex_neq for_block)
  |> List.of_seq
  |> List.sort
       Compare.(
         by
           (tuple (neg int) (tuple int float))
           (fun x ->
             (v.data x).vote, ((if v.appended_by_me x then 0 else 1), v.delivered_at x)))
  |> f IntSet.empty 0 []
  |> Option.map
       (List.sort
          Compare.(
            by
              (tuple (neg int) (tuple int int))
              (fun x ->
                let d = v.data x
                and hash = v.pow_hash x |> Option.value ~default:(0, 0) in
                d.vote, hash)))
;;

let quorum ~k v for_block =
  (* look through the sub blocks (votes) and assemble a quorum if possible. This version
     tries to select the sub blocks that maximize a miner's own rewards.

     We first find all branches of depth k-1 or less. We then sort the branches by own
     reward, assuming that the branch will be confirmed in the future. We then add the
     most valuable branch to the quorum.

     If some (= k') sub blocks are missing after the first iteration, we search for all
     remaining branches that would add 'k or less sub blocks. We sort the branches by the
     amount of reward they would add. We add the most valuable branch an reiterate until
     enough sub blocks are added.

     TODO. For now, we assume constant reward per proof-of-work. Handling the other reward
     schemes correctly requires a restructuring of the code: reward function must become
     an argument of the protocol. *)
  let ht = Hashtbl.create (2 * k)
  and acc = ref []
  and n = ref (k - 1) in
  let included x = Hashtbl.mem ht (Dag.id x) in
  let include_ x =
    assert (not (included x));
    acc := x :: !acc;
    Dag.iterate_ancestors v.votes_only [ x ]
    |> Seq.iter (fun x ->
           if not (included x)
           then (
             Hashtbl.replace ht (Dag.id x) true;
             decr n))
  and reward ?(all = false) x =
    let i = ref 0 in
    let () =
      Dag.iterate_ancestors v.votes_only [ x ]
      |> Seq.iter (fun x ->
             if (not (included x)) && (all || v.appended_by_me x) then incr i)
    in
    !i
  in
  let rec loop () =
    assert (!n >= 0);
    if !n > 0
    then (
      Dag.iterate_descendants v.votes_only [ for_block ]
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
      Some (List.sort (cmp_votes_in_block ~data:v.data ~pow_hash:v.pow_hash) !acc)
  in
  loop ()
;;

let honest_handler ~k v actions preferred = function
  | Activate pow ->
    let head = last_block v preferred in
    (match quorum ~k v head with
    | Some q ->
      let head' =
        actions.extend_dag ~pow (head :: q) { block = (v.data head).block + 1; vote = 0 }
      in
      let () = actions.share head' in
      head'
    | None ->
      let vd = v.data preferred in
      let v = actions.extend_dag ~pow [ preferred ] { vd with vote = vd.vote + 1 } in
      let () = actions.share v in
      v)
  | Deliver consider ->
    (* Prefer longest chain of votes after longest chain of blocks *)
    let p = v.data preferred
    and c = v.data consider in
    if c.block > p.block || (c.block = p.block && c.vote > p.vote)
    then consider
    else preferred
;;

let honest ~k v =
  let v = extend_view v in
  let handler = honest_handler ~k v
  and preferred x = last_block v x in
  { init; handler; preferred }
;;

let constant_block c : _ reward_function =
 fun ~view:v ~assign n -> if v.data n |> is_block then assign c n
;;

let reward ~max_reward_per_block ~discount ~punish ~k : ('env, dag_data) reward_function =
  let k = float_of_int k in
  let c = max_reward_per_block /. k in
  fun ~view:v ~assign ->
    let votes_only = Dag.filter (fun x -> v.data x |> is_vote) v.view in
    fun n ->
      if v.data n |> is_block
      then (
        match Dag.parents votes_only n with
        | [] -> (* Either genesis or k=1 *) assign c n
        | hd :: _ ->
          let depth = (v.data hd).vote in
          let x = if discount then (float_of_int depth +. 1.) /. k *. c else c in
          if punish
          then (
            assign x n;
            Dag.iterate_ancestors votes_only [ hd ] |> Seq.iter (assign x))
          else Dag.iterate_ancestors votes_only [ n ] |> Seq.iter (assign x))
;;

(* TODO: add tests for reward functions *)

let reward_functions ~k =
  let reward = reward ~k ~max_reward_per_block:(float_of_int k) in
  let open Collection in
  empty
  |> add ~info:"1 per confirmed block" "block" (constant_block 1.)
  |> add
       ~info:
         "max k per confirmed block, d/k per pow solution on longest chain of votes (d \
          ∊ 1..k = height since last block)"
       "hybrid"
       (reward ~discount:true ~punish:true)
  |> add
       ~info:"max k per confirmed block, 1 per pow solution on longest chain of votes"
       "punish"
       (reward ~discount:false ~punish:true)
  |> add
       ~info:
         "max k per confirmed block, d/k per confirmed pow solution (d ∊ 1..k = height \
          since last block)"
       "discount"
       (reward ~discount:true ~punish:false)
  |> add
       ~info:"1 per confirmed pow solution"
       "constant"
       (reward ~discount:false ~punish:false)
;;

let block_height v n = (v.data n).block

module PrivateAttack = struct
  let info = "draft withholding attack space"

  type 'env state =
    { public : 'env Dag.vertex (* private/withheld tip of chain *)
    ; private_ : 'env Dag.vertex (* public/defender tip of chain *)
    }

  (* the attacker emulates a defending node. This is the local_view of the defender *)

  let public_visibility (v : _ local_view) x = v.released x

  let public_view (v : _ local_view) =
    { v with
      view = Dag.filter (public_visibility v) v.view
    ; appended_by_me =
        (* The attacker simulates an honest node on the public view. This node should not
           interpret attacker vertices as own vertices. *)
        (fun _ -> false)
    }
    |> extend_view
  ;;

  module Observation = struct
    type t =
      { public_blocks : int (** number of blocks after common ancestor *)
      ; public_depth : int (** number of votes confirming the leading block *)
      ; private_blocks : int (** number of blocks after common ancestor *)
      ; private_depth : int (** number of votes confirming the leading block *)
      ; diff_blocks : int (** private_blocks - public_blocks *)
      ; diff_depth : int (** private_votes - public_votes *)
      }
    [@@deriving fields]

    let length = List.length Fields.names

    let observe v s =
      let private_view = extend_view v in
      let private_depth = (private_view.data s.private_).vote
      and public_depth = (private_view.data s.public).vote in
      let v = private_view in
      let ca =
        Dag.common_ancestor v.view s.private_ s.public |> Option.get |> last_block v
      in
      let ca_height = block_height v ca
      and private_height = block_height v s.private_
      and public_height = block_height v s.public in
      { private_blocks = private_height - ca_height
      ; public_blocks = public_height - ca_height
      ; diff_blocks = private_height - public_height
      ; private_depth
      ; public_depth
      ; diff_depth = private_depth - public_depth
      }
    ;;

    let low =
      { public_blocks = 0
      ; public_depth = 0
      ; private_blocks = 0
      ; private_depth = 0
      ; diff_blocks = min_int
      ; diff_depth = min_int
      }
    ;;

    let high =
      { public_blocks = max_int
      ; public_depth = max_int
      ; private_blocks = max_int
      ; private_depth = max_int
      ; diff_blocks = max_int
      ; diff_depth = max_int
      }
    ;;

    let to_floatarray t =
      let a = Float.Array.make length Float.nan in
      let set conv i field =
        Float.Array.set a i (Fieldslib.Field.get field t |> conv);
        i + 1
      in
      let int = set float_of_int in
      let _ =
        Fields.fold
          ~init:0
          ~public_blocks:int
          ~public_depth:int
          ~private_blocks:int
          ~private_depth:int
          ~diff_blocks:int
          ~diff_depth:int
      in
      a
    ;;

    let of_floatarray =
      let get conv _ i = (fun a -> Float.Array.get a i |> conv), i + 1 in
      let int = get int_of_float in
      fst
        (Fields.make_creator
           0
           ~public_blocks:int
           ~public_depth:int
           ~private_blocks:int
           ~private_depth:int
           ~diff_blocks:int
           ~diff_depth:int)
    ;;

    let to_string t =
      let conv to_s field =
        Printf.sprintf
          "%s: %s"
          (Fieldslib.Field.name field)
          (to_s (Fieldslib.Field.get field t))
      in
      let int = conv string_of_int in
      Fields.to_list
        ~public_blocks:int
        ~public_depth:int
        ~private_blocks:int
        ~private_depth:int
        ~diff_blocks:int
        ~diff_depth:int
      |> String.concat "\n"
    ;;

    let%test _ =
      let run _i =
        let t =
          { public_blocks = Random.bits ()
          ; public_depth = Random.bits ()
          ; private_blocks = Random.bits ()
          ; private_depth = Random.bits ()
          ; diff_blocks = Random.bits ()
          ; diff_depth = Random.bits ()
          }
        in
        t = (to_floatarray t |> of_floatarray)
      in
      List.init 50 run |> List.for_all (fun x -> x)
    ;;
  end

  module Action = struct
    type t =
      | Release
          (** Release up to preferred private block and all withheld votes for this block.
              Used to model honest strategy. *)
      | Override
          (** Publish just enough information to make the defender adopt the chain just
              released. The attacker continues mining the private chain.

              If override is impossible, this still results in a release of withheld
              information. *)
      | Match
          (** Publish just enough information such that the defender observes a tie
              between two chains. The attacker continues mining the private chain.

              If override is impossible, this still results in a release of withheld
              information. *)
      | Wait (** Continue withholding. Always possible. *)
    [@@deriving variants]

    let to_string = Variants.to_name
    let to_int = Variants.to_rank

    let table =
      let add acc var = var.Variantslib.Variant.constructor :: acc in
      Variants.fold ~init:[] ~override:add ~match_:add ~wait:add ~release:add
      |> List.rev
      |> Array.of_list
    ;;

    let of_int i = table.(i)
    let n = Array.length table
  end

  module Agent (A : sig
    val k : int
  end) =
  struct
    (* the attacker emulates a defending node working on a subset of information *)
    let handle_public v actions s event =
      let open A in
      let view = public_view v
      and drop_messages = { actions with share = (fun ?recursive:_ _n -> ()) } in
      { s with public = honest_handler ~k view drop_messages s.public event }
    ;;

    (* the attacker acts honestly on all available information *)

    let handle_private v actions s event =
      let open A in
      let view = extend_view v
      and drop_messages = { actions with share = (fun ?recursive:_ _n -> ()) } in
      { s with private_ = honest_handler ~k view drop_messages s.private_ event }
    ;;

    let prepare v actions state event =
      match event with
      | Activate _ ->
        (* work on private chain *)
        handle_private v actions state event
      | Deliver _ ->
        let state =
          (* simulate defender *)
          handle_public v actions state event
        in
        handle_private v actions state event
    ;;

    let observe = Observation.observe (* TODO move implementation here *)

    let interpret (v : _ local_view) state action =
      let ev = extend_view v in
      let parent n =
        match Dag.parents v.view n with
        | hd :: _ -> Some hd
        | _ -> None
      in
      let match_ ~and_override () =
        let cmp =
          Compare.(
            by (tuple int int) (fun x ->
                let x = v.data x in
                x.block, x.vote))
        in
        let x =
          (* find node to be released backwards from private head *)
          let rec h x x' =
            let d = cmp x state.public in
            if d <= 0
            then if and_override then x' else x
            else h (parent x |> Option.get) x
          in
          h state.private_ state.private_
          (* NOTE: if private height is smaller public height, then private head is marked
             for release. *)
        in
        [ x ]
      in
      match (action : Action.t) with
      | Wait -> [], `PreferPrivate
      | Match -> match_ ~and_override:false (), `PreferPrivate
      | Override -> match_ ~and_override:true (), `PreferPrivate
      | Release -> Dag.leaves v.view (last_block ev state.private_), `PreferPrivate
    ;;

    let conclude v a state (to_release, x) =
      let () = List.iter (a.share ~recursive:true) to_release in
      let state =
        List.fold_left (fun acc el -> handle_public v a acc (Deliver el)) state to_release
      in
      match x with
      | `PreferPrivate -> state
      | `PreferPublic -> { state with private_ = state.public }
    ;;

    let apply v a s action = interpret v s action |> conclude v a s

    let shutdown (v : _ local_view) a s =
      let to_release =
        (* last block plus all confirming votes *)
        Dag.leaves v.view s.private_
      in
      conclude v a s (to_release, `PreferPublic)
    ;;

    (* TODO: expose State.init and State.preferred, but not noop_agent *)
    let noop_node (v : _ local_view) =
      let handler _ s _ = s
      and preferred x = x.private_
      and init ~roots =
        let x = (honest ~k:A.k v).init ~roots in
        { private_ = x; public = x }
      in
      { init; handler; preferred }
    ;;

    let agent' policy (v : _ local_view) =
      let node = noop_node v in
      let handler a s e =
        let s = prepare v a s e in
        observe v s |> policy |> apply v a s
      in
      { node with handler }
    ;;

    let agent p = Node (agent' p)
  end

  module Policies = struct
    let honest_policy _o = Action.Release

    let release_block_policy o =
      let open Observation in
      let open Action in
      if o.private_blocks > o.public_blocks then Override else Wait
    ;;

    let override_block_policy o =
      let open Observation in
      let open Action in
      if o.private_blocks = 0 && o.public_blocks = 0
      then Wait
      else if o.public_blocks = 0
      then Wait
      else Override
    ;;

    let override_catchup_policy o =
      let open Observation in
      let open Action in
      if o.private_blocks = 0 && o.public_blocks = 0
      then Wait
      else if o.public_blocks = 0
      then Wait
      else if o.private_depth = 0 && o.private_blocks = o.public_blocks + 1
      then Override
      else if o.public_blocks = o.private_blocks && o.private_depth = o.public_depth + 1
      then Override
      else if o.private_blocks - o.public_blocks > 10
              (* fork can become really deep for strong attackers. Cut-off shortens time
                 spent in common ancestor computation. *)
      then Override
      else Wait
    ;;
  end

  let policies =
    let open Policies in
    let open Collection in
    empty
    |> add ~info:"emulate honest behaviour" "private-honest" honest_policy
    |> add ~info:"release private block a.s.a.p." "release-block" release_block_policy
    |> add ~info:"override public block a.s.a.p." "override-block" override_block_policy
    |> add
         ~info:"override public head just before defender catches up"
         "override-catchup"
         override_catchup_policy
  ;;
end

module SszLikeAttack = struct
  let info = "SSZ'16-like attack space"

  module State = B_k_lessleader.SszLikeAttack.State

  (* the attacker emulates a defending node. This is the local_view of the defender *)

  let public_visibility _state (v : _ local_view) x = v.released x

  let public_view s (v : _ local_view) =
    { v with
      view = Dag.filter (public_visibility s v) v.view
    ; appended_by_me =
        (* The attacker simulates an honest node on the public view. This node should not
           interpret attacker vertices as own vertices. *)
        (fun _ -> false)
    }
    |> extend_view
  ;;

  (* the attacker works on a subset of the total information: he ignores new defender
     blocks *)

  let private_visibility (s : _ State.t) (v : _ local_view) vertex =
    let ev = extend_view v in
    (* defender votes for the attacker's preferred block *)
    (* || anything mined by the attacker *)
    (* || anything on the common chain *)
    (s.epoch = `Proceed
    && v.data vertex |> is_vote
    && last_block ev vertex $== last_block ev s.private_)
    || v.appended_by_me vertex
    || Dag.partial_order s.common vertex >= 0
  ;;

  let private_view (s : _ State.t) (v : _ local_view) =
    { v with view = Dag.filter (private_visibility s v) v.view } |> extend_view
  ;;

  module Observation = struct
    type t =
      { public_blocks : int (** number of blocks after common ancestor *)
      ; public_depth : int (** number of votes confirming the leading block *)
      ; private_blocks : int (** number of blocks after common ancestor *)
      ; private_depth : int (** number of votes confirming the leading block *)
      ; diff_blocks : int (** private_blocks - public_blocks *)
      ; diff_depth : int (** private_votes - public_votes *)
      }
    [@@deriving fields]

    let length = List.length Fields.names

    let observe v (s : _ State.t) =
      let private_view = extend_view v in
      let private_depth = (private_view.data s.private_).vote
      and public_depth = (private_view.data s.public).vote in
      let v = private_view in
      let ca = s.common |> last_block v in
      let ca_height = block_height v ca
      and private_height = block_height v s.private_
      and public_height = block_height v s.public in
      { private_blocks = private_height - ca_height
      ; public_blocks = public_height - ca_height
      ; diff_blocks = private_height - public_height
      ; private_depth
      ; public_depth
      ; diff_depth = private_depth - public_depth
      }
    ;;

    let low =
      { public_blocks = 0
      ; public_depth = 0
      ; private_blocks = 0
      ; private_depth = 0
      ; diff_blocks = min_int
      ; diff_depth = min_int
      }
    ;;

    let high =
      { public_blocks = max_int
      ; public_depth = max_int
      ; private_blocks = max_int
      ; private_depth = max_int
      ; diff_blocks = max_int
      ; diff_depth = max_int
      }
    ;;

    let to_floatarray t =
      let a = Float.Array.make length Float.nan in
      let set conv i field =
        Float.Array.set a i (Fieldslib.Field.get field t |> conv);
        i + 1
      in
      let int = set float_of_int in
      let _ =
        Fields.fold
          ~init:0
          ~public_blocks:int
          ~public_depth:int
          ~private_blocks:int
          ~private_depth:int
          ~diff_blocks:int
          ~diff_depth:int
      in
      a
    ;;

    let of_floatarray =
      let get conv _ i = (fun a -> Float.Array.get a i |> conv), i + 1 in
      let int = get int_of_float in
      fst
        (Fields.make_creator
           0
           ~public_blocks:int
           ~public_depth:int
           ~private_blocks:int
           ~private_depth:int
           ~diff_blocks:int
           ~diff_depth:int)
    ;;

    let to_string t =
      let conv to_s field =
        Printf.sprintf
          "%s: %s"
          (Fieldslib.Field.name field)
          (to_s (Fieldslib.Field.get field t))
      in
      let int = conv string_of_int in
      Fields.to_list
        ~public_blocks:int
        ~public_depth:int
        ~private_blocks:int
        ~private_depth:int
        ~diff_blocks:int
        ~diff_depth:int
      |> String.concat "\n"
    ;;

    let%test _ =
      let run _i =
        let t =
          { public_blocks = Random.bits ()
          ; public_depth = Random.bits ()
          ; private_blocks = Random.bits ()
          ; private_depth = Random.bits ()
          ; diff_blocks = Random.bits ()
          ; diff_depth = Random.bits ()
          }
        in
        t = (to_floatarray t |> of_floatarray)
      in
      List.init 50 run |> List.for_all (fun x -> x)
    ;;
  end

  module Action = B_k_lessleader.SszLikeAttack.Action

  module Agent (A : sig
    val k : int
  end) =
  struct
    (* the attacker emulates a defending node. This describes the defender node *)
    let handle_public v actions (s : _ State.t) event =
      let view = public_view s v
      and drop_messages = { actions with share = (fun ?recursive:_ _n -> ()) } in
      let public = honest_handler ~k:A.k view drop_messages s.public event in
      State.update v ~public s
    ;;

    let handle_private v actions (s : _ State.t) event =
      let view = private_view s v
      and drop_messages = { actions with share = (fun ?recursive:_ _n -> ()) } in
      let private_ = honest_handler ~k:A.k view drop_messages s.private_ event in
      State.update v ~private_ s
    ;;

    let prepare v actions state event =
      match event with
      | Activate _ ->
        (* work on private chain *)
        handle_private v actions state event
      | Deliver x ->
        let state =
          (* simulate defender *)
          handle_public v actions state event
        in
        (* deliver visible (not ignored) messages *)
        if private_visibility state v x
        then handle_private v actions state event
        else state
    ;;

    let observe = Observation.observe (* TODO move implementation here *)

    let interpret (v : _ local_view) (s : _ State.t) action =
      let parent n =
        match Dag.parents v.view n with
        | hd :: _ -> Some hd
        | _ -> None
      in
      let release kind =
        let cmp =
          Compare.(
            by (tuple int int) (fun x ->
                let x = v.data x in
                x.block, x.vote))
        in
        (* find node to be released backwards from private head *)
        let rec h x x' =
          let d = cmp x s.public in
          if d <= 0
          then (
            match kind with
            | `Override -> x'
            | `Match -> x)
          else h (parent x |> Option.get) x
        in
        [ h s.private_ s.private_ ]
        (* NOTE: if private height is smaller public height, then private head is marked
           for release. *)
      in
      match (action : Action.t) with
      | Adopt_Proceed -> [], State.update v ~epoch:`Proceed ~private_:s.public s
      | Adopt_Prolong -> [], State.update v ~epoch:`Prolong ~private_:s.public s
      | Match_Proceed -> release `Match, State.update v ~epoch:`Proceed s
      | Match_Prolong -> release `Match, State.update v ~epoch:`Prolong s
      | Override_Proceed -> release `Override, State.update v ~epoch:`Proceed s
      | Override_Prolong -> release `Override, State.update v ~epoch:`Prolong s
      | Wait_Proceed -> [], State.update v ~epoch:`Proceed s
      | Wait_Prolong -> [], State.update v ~epoch:`Prolong s
    ;;

    let conclude v a (to_release, s) =
      let () = List.iter (a.share ~recursive:true) to_release in
      List.fold_left (fun acc el -> handle_public v a acc (Deliver el)) s to_release
    ;;

    let apply v a s action = interpret v s action |> conclude v a

    let shutdown (v : _ local_view) a (s : _ State.t) =
      let to_release =
        (* last block plus all confirming votes *)
        s.private_ :: (Dag.iterate_descendants v.view [ s.private_ ] |> List.of_seq)
      in
      let s = conclude v a (to_release, s) in
      State.update v ~private_:s.public s
    ;;

    (* TODO: expose State.init and State.preferred, but not noop_agent *)
    let noop_node (v : _ local_view) =
      let handler _ s _ = s
      and preferred (x : _ State.t) = x.private_
      and init ~roots =
        let x = (honest ~k:A.k v).init ~roots in
        State.init ~epoch:`Prolong x
      in
      { init; handler; preferred }
    ;;

    let agent' policy (v : _ local_view) =
      let node = noop_node v in
      let handler a s e =
        let s = prepare v a s e in
        observe v s |> policy |> apply v a s
      in
      { node with handler }
    ;;

    let agent p = Node (agent' p)
  end

  module Policies = struct
    let honest o =
      let open Observation in
      let open Action in
      if o.public_blocks > 0 then Adopt_Proceed else Override_Proceed
    ;;

    let release_block o =
      let open Observation in
      let open Action in
      if o.private_blocks < o.public_blocks
      then Adopt_Proceed
      else if o.private_blocks > o.public_blocks
      then Override_Proceed
      else Wait_Proceed
    ;;

    let override_block o =
      let open Observation in
      let open Action in
      if o.private_blocks < o.public_blocks
      then Adopt_Proceed
      else if o.private_blocks = 0 && o.public_blocks = 0
      then Wait_Proceed
      else if o.public_blocks = 0
      then Wait_Proceed
      else Override_Proceed
    ;;

    let override_catchup o =
      let open Observation in
      let open Action in
      if o.private_blocks < o.public_blocks
      then Adopt_Proceed
      else if o.private_blocks = 0 && o.public_blocks = 0
      then Wait_Proceed
      else if o.public_blocks = 0
      then Wait_Proceed
      else if o.private_depth = 0 && o.private_blocks = o.public_blocks + 1
      then Override_Proceed
      else if o.public_blocks = o.private_blocks && o.private_depth = o.public_depth + 1
      then Override_Proceed
      else if o.private_blocks - o.public_blocks > 10
              (* fork can become really deep for strong attackers. Cut-off shortens time
                 spent in common ancestor computation. *)
      then Override_Proceed
      else Wait_Proceed
    ;;
  end

  let policies =
    let open Collection in
    let open Policies in
    empty
    |> add ~info:"emulate honest behaviour" "honest" honest
    |> add
         ~info:"release private block a.s.a.p., inspired from PrivateAttack"
         "release-block"
         release_block
    |> add
         ~info:"override public block a.s.a.p., inspired from PrivateAttack"
         "override-block"
         override_block
    |> add
         ~info:
           "override public head just before defender catches up, inspired from \
            PrivateAttack"
         "override-catchup"
         override_catchup
  ;;
end

let attacks ~k =
  let a =
    let module A =
      PrivateAttack.Agent (struct
        let k = k
      end)
    in
    Collection.map
      (fun { key; info; it } ->
        { key = "ssz-" ^ key; info = PrivateAttack.info ^ "; " ^ info; it = A.agent it })
      PrivateAttack.policies
  and b =
    let module A =
      SszLikeAttack.Agent (struct
        let k = k
      end)
    in
    Collection.map
      (fun { key; info; it } ->
        { key = "ssz-" ^ key; info = SszLikeAttack.info ^ "; " ^ info; it = A.agent it })
      SszLikeAttack.policies
  in
  Collection.concat a b
;;

let protocol ~k =
  { key = "tailstorm"
  ; info = "Tailstorm with k=" ^ string_of_int k
  ; pow_per_block = k
  ; honest = honest ~k
  ; dag_validity = dag_validity ~k
  ; dag_roots
  ; describe
  ; height
  ; reward_functions = reward_functions ~k
  ; attacks = attacks ~k
  }
;;

let%test "convergence" =
  let open Simulator in
  let propagation_delay = Distributions.exponential ~ev:1. in
  let test (k, activation_delay, height) =
    let network = Network.T.symmetric_clique ~activation_delay ~propagation_delay 32 in
    let env = all_honest network (protocol ~k) |> init in
    loop ~activations:(1000 * k) env;
    Array.to_seq env.nodes
    |> Seq.map (fun (Node x) -> x.preferred x.state)
    |> Dag.common_ancestor' env.global.view
    |> function
    | None -> false
    | Some n ->
      let bheight = (Dag.data n).value.block in
      if bheight < height
      then (
        Printf.eprintf "k: %i\theight: %i\texpected: %i\n" k bheight height;
        false)
      else true
    (* more than 900 blocks in a sequence imply less than 10% orphans. *)
  in
  List.for_all
    test
    [ 08, 10., 900 (* good condition, 10% orphans *)
    ; 08, 01., 700 (* bad conditions, 30% orphans *)
    ; 32, 01., 900
      (* bad conditions, 10% orphans, high k *)
    ]
;;
