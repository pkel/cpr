open Cpr_lib
open Protocol

type block = { height : int }

type node =
  | Vote of int
  | Block of block

let is_vote = function
  | Vote _ -> true
  | _ -> false
;;

let is_block = function
  | Block _ -> true
  | _ -> false
;;

let dag_validity ~k (v : _ global_view) n =
  let has_pow n = v.pow_hash n |> Option.is_some
  and pow_hash n = v.pow_hash n |> Option.get in
  match v.data n, Dag.parents v.view n with
  | Vote _, [ p ] -> has_pow n && v.data p |> is_block
  | Block b, pblock :: vote0 :: votes ->
    (match v.data pblock, v.data vote0 with
    | Block p, Vote leader ->
      let ordered_votes, _, nvotes =
        List.fold_left
          (fun (ok, h, i) n ->
            let h' = pow_hash n in
            v.data n |> is_vote && h' > h && ok, h', i + 1)
          (true, pow_hash vote0, 1)
          votes
      in
      p.height + 1 = b.height
      && nvotes = k
      && ordered_votes
      && v.signed_by n = Some leader
    | _ -> false)
  | _ -> false
;;

let dag_roots = [ Block { height = 0 } ]

let init ~roots =
  match roots with
  | [ genesis ] -> genesis
  | _ -> failwith "invalid roots"
;;

type ('env, 'data) extended_view =
  { view : 'env Dag.view
  ; data : 'env Dag.node -> 'data
  ; votes_only : 'env Dag.view
  ; blocks_only : 'env Dag.view
  ; delivered_at : 'env Dag.node -> float
  ; pow_hash : 'env Dag.node -> (int * int) option
  ; appended_by_me : 'env Dag.node -> bool
  ; released : 'env Dag.node -> bool
  ; my_id : int
  }

let max_pow_hash = max_int, max_int

let extend_view (x : _ Protocol.local_view) =
  { view = x.view
  ; data = x.data
  ; votes_only = Dag.filter (fun n -> x.data n |> is_vote) x.view
  ; blocks_only = Dag.filter (fun n -> x.data n |> is_block) x.view
  ; delivered_at = x.delivered_at
  ; appended_by_me = x.appended_by_me
  ; pow_hash = x.pow_hash
  ; released = x.released
  ; my_id = x.my_id
  }
;;

type exn += Invalid_DAG of string lazy_t

let () =
  Printexc.register_printer (function
      | Invalid_DAG (lazy m) -> Some m
      | _ -> None)
;;

let invalid_dag v n msg (type a) : a =
  let msg = lazy (Format.asprintf "Invalid_DAG: %s: %a" msg (Dag.debug_pp v.view) n) in
  raise (Invalid_DAG msg)
;;

let block_height_exn v n =
  match v.data n with
  | Block b -> b.height
  | _ -> invalid_dag v n "not a block"
;;

let last_block v n =
  match v.data n with
  | Block _ -> n
  | Vote _ ->
    (match Dag.parents v.view n with
    | [ n ] -> n
    | _ -> failwith "invalid dag")
;;

let leader_hash_exn v n =
  if not (is_block (v.data n)) then raise (Invalid_argument "not a block");
  match Dag.parents v.view n with
  | _b :: v0 :: _ ->
    (match v.pow_hash v0 with
    | Some x -> x
    | None -> raise (Invalid_argument "invalid dag / vote"))
  | _ ->
    (* happens for genesis node *)
    max_pow_hash
;;

let first ?(skip_to = fun _ -> true) by n l =
  let a = Array.of_list l in
  if Array.length a < n
  then None
  else (
    let () = Array.sort (fun a b -> compare (by a) (by b)) a in
    let i = ref 0 in
    while !i < Array.length a && not (skip_to a.(!i)) do
      incr i
    done;
    if Array.length a - !i < n
    then None
    else (
      let l = ref [] in
      for j = 0 to n - 1 do
        l := a.(n - 1 + !i - j) :: !l
      done;
      Some !l))
;;

let compare_blocks v =
  let open Compare in
  let cmp =
    by int (block_height_exn v)
    $ by int (fun n -> List.length (Dag.children v.votes_only n))
    $ by (tuple int int |> inv) (leader_hash_exn v)
    $ by (inv float) v.delivered_at
  in
  skip_eq Dag.node_eq cmp
;;

let update_head v ~preferred ~consider =
  if compare_blocks v consider preferred > 0 then consider else preferred
;;

let quorum ~k v b =
  let pow_hash_exn n = v.pow_hash n |> Option.get in
  let my_hash, replace_hash, mine, nmine, theirs, ntheirs =
    List.fold_left
      (fun (my_hash, replace_hash, mine, nmine, theirs, ntheirs) n ->
        match v.data n with
        | Vote _ ->
          if v.appended_by_me n
          then
            ( min my_hash (pow_hash_exn n)
            , replace_hash
            , n :: mine
            , nmine + 1
            , theirs
            , ntheirs )
          else my_hash, replace_hash, mine, nmine, n :: theirs, ntheirs + 1
        | Block _ ->
          my_hash, min replace_hash (leader_hash_exn v n), mine, nmine, theirs, ntheirs)
      (max_pow_hash, max_pow_hash, [], 0, [], 0)
      (Dag.children v.view b)
  in
  if replace_hash <= my_hash || nmine + ntheirs < k
  then (* fast path *) None
  else if nmine >= k
  then first pow_hash_exn k mine
  else (
    let theirs, ntheirs =
      List.fold_left
        (fun (theirs, ntheirs) vote ->
          if pow_hash_exn vote > my_hash
          then vote :: theirs, ntheirs + 1
          else theirs, ntheirs)
        ([], 0)
        theirs
    in
    if ntheirs < k - nmine
    then (* fast path *) None
    else (
      let theirs = first v.delivered_at (k - nmine) theirs |> Option.get in
      mine @ theirs
      |> List.sort (fun a b -> compare (pow_hash_exn a) (pow_hash_exn b))
      |> Option.some))
;;

let honest ~k v actions =
  let propose b =
    quorum ~k v b
    |> Option.map (fun q ->
           let block =
             actions.extend_dag
               ~sign:true
               (b :: q)
               (Block { height = block_height_exn v b + 1 })
           in
           actions.share block;
           block)
  in
  fun preferred -> function
    | Activate pow ->
      let vote = actions.extend_dag ~pow [ preferred ] (Vote v.my_id) in
      actions.share vote;
      Option.value ~default:preferred (propose preferred)
    | Deliver n ->
      (* We only prefer blocks. For received votes, reconsider parent block. *)
      let b = last_block v n in
      let consider =
        (* propose if possible *)
        match propose b with
        | Some b' -> [ b; b' ]
        | None -> [ b ]
      in
      (* prefer best block of all blocks involved *)
      List.fold_left
        (fun preferred consider -> update_head v ~preferred ~consider)
        preferred
        consider
;;

let protocol ~k =
  let honest v =
    let handler = honest ~k (extend_view v)
    and preferred x = x in
    { init; handler; preferred }
  in
  { honest; dag_validity = dag_validity ~k; dag_roots }
;;

let%test "convergence" =
  let open Simulator in
  let test k params height =
    let env = all_honest params (protocol ~k) |> init in
    loop params env;
    Array.to_seq env.nodes
    |> Seq.map (fun (Node x) -> x.preferred x.state)
    |> Dag.common_ancestor'
         (Dag.filter (fun x -> is_block (Dag.data x).value) env.global.view)
    |> function
    | None -> false
    | Some n ->
      (match (Dag.data n).value with
      | Block b ->
        if b.height < height
        then (
          Printf.eprintf "k: %i\theight: %i\texpected: %i\n" k b.height height;
          false)
        else true (* more than 900 blocks in a sequence imply less than 10% orphans. *)
      | _ -> failwith "invalid dag")
  in
  let delay = Distributions.exponential ~ev:1. in
  List.for_all
    (fun (k, activation_delay, height) ->
      let network = Network.homogeneous ~delay 32 in
      test k { network; activations = 1000 * k; activation_delay } height)
    [ 08, 10., 900 (* good condition, 10% orphans *)
    ; 08, 01., 700 (* bad conditions, 30% orphans *)
    ; 32, 01., 900
      (* bad conditions, 10% orphans, high k *)
    ]
;;

let constant_pow c : ('env, node) reward_function =
 fun ~view:v ~assign n ->
  match v.pow_hash n with
  | Some _ -> assign c n
  | None -> ()
;;

let constant_block c : ('env, node) reward_function =
 fun ~view:v ~assign n -> if v.data n |> is_block then assign c n
;;

module PrivateAttack = struct
  type 'env state =
    { public : 'env Dag.node
    ; private_ : 'env Dag.node
    }

  type ('env, 'data) strategic_view =
    { public_view : ('env, 'data) extended_view
    ; private_view : ('env, 'data) extended_view
    }

  let strategic_view (v : _ local_view) =
    { public_view = extend_view { v with view = Dag.filter v.released v.view }
    ; private_view = extend_view v
    }
  ;;

  module Observation = struct
    type t =
      | PublicBlocks (* number of blocks after common ancestor *)
      | PublicVotes (* number of votes confirming the leading block *)
      | PrivateBlocks (* number of blocks after common ancestor *)
      | PrivateVotes (* number of votes confirming the leading block *)
      | DiffBlocks (* PrivateBlocks - PublicBlocks *)
      | DiffVotes (* PrivateVotes - PublicVotes *)
      | Lead (* 1 if attacker is truthful leader on leading public block, -1 otherwise *)
    [@@deriving enumerate, variants]

    let n = List.length all

    let observe_field v s =
      let private_votes = Dag.children v.private_view.votes_only s.private_ |> List.length
      and public_votes = Dag.children v.public_view.votes_only s.public |> List.length in
      let v = v.private_view in
      let lead =
        match Dag.children v.votes_only s.public with
        | [] -> false
        | votes ->
          let leader =
            first (fun n -> v.pow_hash n |> Option.get) 1 votes |> Option.get |> List.hd
          in
          v.appended_by_me leader
      and ca = Dag.common_ancestor v.view s.private_ s.public |> Option.get in
      let ca_height = block_height_exn v ca
      and private_height = block_height_exn v s.private_
      and public_height = block_height_exn v s.public in
      function
      | PublicBlocks -> public_height - ca_height
      | PrivateBlocks -> private_height - ca_height
      | DiffBlocks -> private_height - public_height
      | PublicVotes -> public_votes
      | PrivateVotes -> private_votes
      | DiffVotes -> private_votes - public_votes
      | Lead -> if lead then 1 else -1
    ;;

    let read a t =
      let i = Variants.to_rank t in
      Float.Array.get a i |> int_of_float
    ;;

    let observe_into v s a =
      let observe_field = observe_field v s in
      List.iteri
        (fun i t ->
          let x = observe_field t |> float_of_int in
          Float.Array.set a i x)
        all
    ;;

    let observe v s =
      let a = Float.Array.make n Float.nan in
      observe_into v s a;
      a
    ;;

    let to_string_hum a =
      List.map (fun t -> Printf.sprintf "%s: %d" (Variants.to_name t) (read a t)) all
      |> String.concat "\n"
    ;;
  end

  (* release a given node and all it's dependencies recursively *)
  let rec release v actions ns =
    List.iter
      (fun n ->
        if not (v.released n)
        then (
          actions.share n;
          release v actions (Dag.parents v.view n)))
      ns
  ;;

  type action =
    | Adopt (** Abort withholding, adopt defender's chain. Always possible. *)
    | Override
        (** Publish just enough information to make the defender adopt the chain just
            released.

            If override is impossible, this results in a release of withheld information
            and attacker adopting the defender's chain. *)
    | Match
        (** Publish just enough information such that the defender observes a tie between
            two chains.

            If match is impossible, this results in a release of withheld information and
            attacker adopting the defender's chain. *)
    | Wait (** Continue withholding. Always possible. *)
  [@@deriving enumerate, variants]

  let selfish_policy obs =
    let open Observation in
    let v = read obs in
    if v PrivateBlocks = 0 && v PublicBlocks = 0
    then Wait
    else (
      let d = compare (v PrivateBlocks, v PrivateVotes) (v PublicBlocks, v PublicVotes) in
      if d < 0 then Adopt else if v PublicBlocks = 0 then Wait else Override)
  ;;

  let selfish_tactic v state =
    if state.private_ $== state.public
    then Wait
    else (
      let d = compare_blocks v.private_view state.private_ state.public in
      if d < 0
      then Adopt
      else (
        let ca =
          Dag.common_ancestor v.private_view.blocks_only state.private_ state.public
          |> Option.get
        in
        if ca $== state.public then Wait else Override))
  ;;

  let parent_block v n =
    match Dag.parents v.view n with
    | hd :: _ when v.data hd |> is_block -> Some hd
    | _ -> None
  ;;

  let apply_action ~k v actions state action =
    let match_ ~and_override () =
      let height, nvotes =
        let target =
          (block_height_exn v.public_view state.public * k)
          + List.length (Dag.children v.public_view.votes_only state.public)
          + if and_override then 1 else 0
        in
        target / k, target mod k
      in
      let block =
        (* find block to be released backwards from private head *)
        let rec h b =
          if block_height_exn v.private_view b <= height
          then b
          else parent_block v.private_view b |> Option.get |> h
        in
        h state.private_
        (* NOTE: if private height is smaller public height, then private head is marked
           for release. *)
      in
      let () =
        let v = v.private_view in
        let votes = Dag.children v.votes_only block in
        match first v.delivered_at nvotes votes with
        | Some subset -> release v actions (block :: subset)
        | None ->
          (* not enough votes, release all *)
          release v actions (block :: votes)
      in
      update_head v.private_view ~preferred:state.private_ ~consider:state.public
    in
    let private_ =
      match action with
      | Adopt -> state.public
      | Wait -> state.private_
      | Match -> match_ ~and_override:false ()
      | Override -> match_ ~and_override:true ()
    in
    { state with private_ }
  ;;

  let noop_tactic _ _ = Wait

  let strategic tactic ~k (v : _ local_view) =
    let v = strategic_view v in
    let handler actions state event =
      let state =
        let withhold = { actions with share = (fun _n -> ()) } in
        match event with
        | Activate _ ->
          { state with private_ = honest ~k v.private_view withhold state.private_ event }
        | Deliver _ ->
          { state with public = honest ~k v.public_view withhold state.public event }
      in
      tactic v state |> apply_action ~k v actions state
    and preferred x = x.private_
    and init ~roots =
      let s = init ~roots in
      { public = s; private_ = s }
    in
    { init; handler; preferred }
  ;;
end
