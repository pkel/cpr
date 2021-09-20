open Cpr_lib

type dag_data =
  { block : int
  ; vote : int
  }

let is_vote h = h.vote > 0
let is_block h = h.vote = 0

let describe h =
  let ty = if is_vote h then "vote" else "block" in
  Printf.sprintf "%s (%i|%i)" ty h.block h.vote
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
      List.map
        (fun x ->
          let d = v.data x
          and hash = v.pow_hash x |> Option.value ~default:(0, 0) in
          d.vote, hash)
        votes
      |> is_sorted ~unique:true Compare.(tuple (neg int) (tuple int int))
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

let quorum ~k v for_block =
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

let honest ~k v actions preferred = function
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
  let handler = honest ~k v
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
       ~info:"1 per confirmed pow solution"
       "discount"
       (reward ~discount:true ~punish:false)
  |> add
       ~info:
         "max k per confirmed block, d/k per pow solution on longest chain of votes (d \
          ∊ 1..k = height since last block)"
       "constant"
       (reward ~discount:false ~punish:false)
;;

let block_height v n = (v.data n).block

module PrivateAttack = struct
  open PrivateAttack

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
    else if o.public_blocks = o.private_blocks && o.private_depth = o.private_depth + 1
    then Override
    else if o.private_blocks - o.public_blocks > 10
            (* fork can become really deep for strong attackers. Cut-off shortens time
               spent in common ancestor computation. *)
    then Override
    else Wait
  ;;

  let policies =
    [ "honest", honest_policy
    ; "release_block", release_block_policy
    ; "override_block", override_block_policy
    ; "override_catchup", override_catchup_policy
    ]
  ;;

  let override_block_policy' (v : _ local_view) state =
    let v = extend_view v in
    let priv = last_block v state.private_
    and publ = last_block v state.public in
    let open Action in
    if priv $== publ
    then Wait
    else (
      let ca =
        Dag.common_ancestor v.view state.private_ state.public
        |> Option.get
        |> last_block v
      in
      if ca $== publ then Wait else Override)
  ;;

  let apply_action ~k:_ (v : _ local_view) ~release state =
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
          if d <= 0 then if and_override then x' else x else h (parent x |> Option.get) x
        in
        h state.private_ state.private_
        (* NOTE: if private height is smaller public height, then private head is marked
           for release. *)
      in
      release_recursive v release [ x ]
    in
    fun a ->
      match (a : Action.t) with
      | Wait -> `PreferPrivate
      | Match ->
        match_ ~and_override:false ();
        `PreferPrivate
      | Override ->
        match_ ~and_override:true ();
        `PreferPrivate
      | Release ->
        Dag.leaves v.view (last_block ev state.private_) |> release_recursive v release;
        `PreferPrivate
  ;;

  let lift_policy p (v : _ local_view) state : Action.t = Observation.observe v state |> p

  let tactic_of_policy ~k p v ~release state =
    (lift_policy p) v state |> apply_action ~k v ~release state
  ;;

  let tactic_of_policy' ~k p v ~release state =
    p v state |> apply_action ~k v ~release state
  ;;

  let attack ~k p = Node (attack (honest ~k) (tactic_of_policy ~k p))
  and attack' ~k p = Node (attack (honest ~k) (tactic_of_policy' ~k p))
end

let attacks ~k =
  let open Collection in
  empty
  |> add
       ~info:"Private attack with honest policy"
       "private-honest"
       PrivateAttack.(attack ~k honest_policy)
  |> add
       ~info:"Private attack: release private block a.s.a.p."
       "private-release-block"
       PrivateAttack.(attack ~k release_block_policy)
  |> add
       ~info:"Private attack: override public block a.s.a.p."
       "private-override-block"
       PrivateAttack.(attack ~k override_block_policy)
  |> add
       ~info:
         "Private attack: override public block a.s.a.p. (alternative policy \
          implementation)"
       "private-override-block-alt"
       PrivateAttack.(attack' ~k override_block_policy')
  |> add
       ~info:"Private attack: override public head just before defender catches up"
       "private-override-catchup"
       PrivateAttack.(attack ~k override_catchup_policy)
;;

let protocol ~k =
  { key = "george"
  ; info = "George's protocol with k=" ^ string_of_int k
  ; pow_per_block = k
  ; honest = honest ~k
  ; dag_validity = dag_validity ~k
  ; dag_roots
  ; describe
  ; reward_functions = reward_functions ~k
  ; attacks = attacks ~k
  }
;;

let%test "convergence" =
  let open Simulator in
  let delay = Distributions.exponential ~ev:1. in
  let network = Network.homogeneous ~delay 32 in
  let test (k, activation_delay, height) =
    let params = { activations = 1000 * k; activation_delay } in
    let env = all_honest params network (protocol ~k) |> init in
    loop params env;
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
