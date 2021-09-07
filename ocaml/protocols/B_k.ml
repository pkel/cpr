open Cpr_lib

type block = { height : int }

type dag_data =
  | Vote of int
  | Block of block

let describe = function
  | Vote voter -> "vote by " ^ string_of_int voter
  | Block { height } -> "block " ^ string_of_int height
;;

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

let block_height_exn v n =
  match v.data n with
  | Block b -> b.height
  | _ ->
    let info _v = [] in
    Dag.Exn.raise v.view info [ n ] "not a block"
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
    $ by (tuple int int |> neg) (leader_hash_exn v)
    $ by (neg float) v.delivered_at
  in
  skip_eq Dag.vertex_eq cmp
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
      mine @ theirs |> List.sort Compare.(by (tuple int int) pow_hash_exn) |> Option.some))
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
  { honest; dag_validity = dag_validity ~k; dag_roots; describe }
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

let constant_pow c : ('env, dag_data) reward_function =
 fun ~view:v ~assign n ->
  match v.pow_hash n with
  | Some _ -> assign c n
  | None -> ()
;;

let constant_block c : ('env, dag_data) reward_function =
 fun ~view:v ~assign n -> if v.data n |> is_block then assign c n
;;

module PrivateAttack = struct
  open PrivateAttack

  module Observation = struct
    type t =
      { public_blocks : int (** number of blocks after common ancestor *)
      ; public_votes : int (** number of votes confirming the leading block *)
      ; private_blocks : int (** number of blocks after common ancestor *)
      ; private_votes : int (** number of votes confirming the leading block *)
      ; diff_blocks : int (** private_blocks - public_blocks *)
      ; diff_votes : int (** private_votes - public_votes *)
      ; lead : bool (** attacker is truthful leader on leading public block *)
      }
    [@@deriving fields]

    let length = List.length Fields.names

    let observe v s =
      let private_view = extend_view v
      and public_view = extend_view (public_view v) in
      let private_votes = Dag.children private_view.votes_only s.private_ |> List.length
      and public_votes = Dag.children public_view.votes_only s.public |> List.length in
      let v = private_view in
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
      { private_blocks = private_height - ca_height
      ; public_blocks = public_height - ca_height
      ; diff_blocks = private_height - public_height
      ; private_votes
      ; public_votes
      ; diff_votes = private_votes - public_votes
      ; lead
      }
    ;;

    let low =
      { public_blocks = 0
      ; public_votes = 0
      ; private_blocks = 0
      ; private_votes = 0
      ; diff_blocks = min_int
      ; diff_votes = min_int
      ; lead = false
      }
    ;;

    let high =
      { public_blocks = max_int
      ; public_votes = max_int
      ; private_blocks = max_int
      ; private_votes = max_int
      ; diff_blocks = max_int
      ; diff_votes = max_int
      ; lead = true
      }
    ;;

    let to_floatarray t =
      let a = Float.Array.make length Float.nan in
      let set conv i field =
        Float.Array.set a i (Fieldslib.Field.get field t |> conv);
        i + 1
      in
      let int = set float_of_int
      and bool = set (fun x -> if x then 1. else 0.) in
      let _ =
        Fields.fold
          ~init:0
          ~public_blocks:int
          ~public_votes:int
          ~private_blocks:int
          ~private_votes:int
          ~diff_blocks:int
          ~diff_votes:int
          ~lead:bool
      in
      a
    ;;

    let of_floatarray =
      let get conv _ i = (fun a -> Float.Array.get a i |> conv), i + 1 in
      let int = get int_of_float
      and bool =
        get (fun f ->
            match int_of_float f with
            | 0 -> false
            | _ -> true)
      in
      fst
        (Fields.make_creator
           0
           ~public_blocks:int
           ~public_votes:int
           ~private_blocks:int
           ~private_votes:int
           ~diff_blocks:int
           ~diff_votes:int
           ~lead:bool)
    ;;

    let to_string t =
      let conv to_s field =
        Printf.sprintf
          "%s: %s"
          (Fieldslib.Field.name field)
          (to_s (Fieldslib.Field.get field t))
      in
      let int = conv string_of_int
      and bool = conv string_of_bool in
      Fields.to_list
        ~public_blocks:int
        ~public_votes:int
        ~private_blocks:int
        ~private_votes:int
        ~diff_blocks:int
        ~diff_votes:int
        ~lead:bool
      |> String.concat "\n"
    ;;

    let%test _ =
      let run _i =
        let t =
          { public_blocks = Random.bits ()
          ; public_votes = Random.bits ()
          ; private_blocks = Random.bits ()
          ; private_votes = Random.bits ()
          ; diff_blocks = Random.bits ()
          ; diff_votes = Random.bits ()
          ; lead = Random.bool ()
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

  let selfish_policy o =
    let open Observation in
    let open Action in
    if o.private_blocks = 0 && o.public_blocks = 0
    then Wait
    else if o.public_blocks = 0
    then Wait
    else Override
  ;;

  let policies = [ "honest", honest_policy; "selfish", selfish_policy ]

  let selfish_policy' v state =
    let v = extend_view v in
    let open Action in
    if state.private_ $== state.public
    then Wait
    else (
      let ca =
        Dag.common_ancestor v.blocks_only state.private_ state.public |> Option.get
      in
      if ca $== state.public then Wait else Override)
  ;;

  let apply_action ~k v ~release state =
    let parent_block v n =
      match Dag.parents v.view n with
      | hd :: _ when v.data hd |> is_block -> Some hd
      | _ -> None
    in
    let match_ ~and_override () =
      let height, nvotes =
        let v = public_view v |> extend_view in
        let target =
          (block_height_exn v state.public * k)
          + List.length (Dag.children v.votes_only state.public)
          + if and_override then 1 else 0
        in
        target / k, target mod k
      in
      let ev = extend_view v in
      let block =
        (* find block to be released backwards from private head *)
        let rec h b =
          if block_height_exn ev b <= height
          then b
          else parent_block ev b |> Option.get |> h
        in
        h state.private_
        (* NOTE: if private height is smaller public height, then private head is marked
           for release. *)
      in
      let votes = Dag.children ev.votes_only block in
      match first v.delivered_at nvotes votes with
      | Some subset -> release_recursive v release (block :: subset)
      | None ->
        (* not enough votes, release all *)
        release_recursive v release (block :: votes)
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
        release_recursive v release [ state.private_ ];
        Dag.children v.view state.private_ |> List.iter release;
        `PreferPrivate
  ;;

  let lift_policy p (v : _ local_view) state : Action.t = Observation.observe v state |> p

  let tactic_of_policy ~k p v ~release state =
    (lift_policy p) v state |> apply_action ~k v ~release state
  ;;

  let tactic_of_policy' ~k p v ~release state =
    p v state |> apply_action ~k v ~release state
  ;;

  let attack ~k p = attack (protocol ~k).honest (tactic_of_policy ~k p)
  and attack' ~k p = attack (protocol ~k).honest (tactic_of_policy' ~k p)
end
