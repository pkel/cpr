open Cpr_lib

type block = { height : int }

type dag_data =
  | Vote of { height : int }
  | Block of block

let describe = function
  | Vote _ -> "vote"
  | Block { height } -> "block " ^ string_of_int height
;;

let height = function
  | Vote x -> x.height
  | Block x -> x.height
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
  match v.pow_hash n, v.data n, Dag.parents v.view n with
  | Some _, Vote x, [ p ] ->
    let pd = v.data p in
    is_block pd && x.height = height pd
  | Some _, Block b, [ pblock ] when k = 1 ->
    (match v.data pblock with
    | Block p -> p.height + 1 = b.height
    | _ -> false)
  | Some _, Block b, pblock :: vote0 :: votes ->
    (match v.data pblock with
    | Block p ->
      let ordered_votes, _, nvotes =
        List.fold_left
          (fun (ok, h, i) n ->
            let h' = v.pow_hash n |> Option.get in
            v.data n |> is_vote && h' > h && ok, h', i + 1)
          (true, v.pow_hash vote0 |> Option.get, 1)
          votes
      in
      p.height + 1 = b.height && nvotes = k - 1 && ordered_votes
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

let last_block v n =
  match v.data n with
  | Block _ -> n
  | Vote _ ->
    (match Dag.parents v.view n with
    | [ n ] -> n
    | _ -> failwith "invalid dag")
;;

let block_data_exn v node =
  match v.data node with
  | Block b -> b
  | _ -> raise (Invalid_argument "not a block")
;;

let block_height_exn v n = (block_data_exn v n).height

let compare_blocks v =
  let open Compare in
  let cmp =
    by int (block_height_exn v)
    $ by int (fun n -> List.length (Dag.children v.votes_only n))
    $ by int (fun n -> if v.appended_by_me n then 1 else 0)
    $ by (neg float) v.delivered_at
  in
  skip_eq Dag.vertex_eq cmp
;;

let update_head v ~preferred ~consider =
  if compare_blocks v consider preferred > 0 then consider else preferred
;;

let honest_handler ~k v actions preferred =
  let pow_hash_exn n = v.pow_hash n |> Option.get in
  function
  | Activate pow ->
    let votes = Dag.children v.votes_only preferred in
    if List.length votes >= k - 1
    then (
      let head = block_data_exn v preferred in
      let head' =
        actions.extend_dag
          ~pow
          (preferred
          :: (first
                Compare.(
                  by
                    (tuple (neg bool) float)
                    (fun n -> v.appended_by_me n, v.delivered_at n))
                (k - 1)
                votes
             |> Option.get
             |> List.sort Compare.(by (tuple int int) pow_hash_exn)))
          (Block { height = head.height + 1 })
      in
      actions.share head';
      head')
    else (
      let height = v.data preferred |> height in
      let vote = actions.extend_dag ~pow [ preferred ] (Vote { height }) in
      actions.share vote;
      preferred)
  | Deliver n ->
    (* We only prefer blocks. For received votes, reconsider parent block. *)
    let consider = last_block v n in
    update_head v ~preferred ~consider
;;

let honest ~k v =
  let handler = honest_handler ~k (extend_view v)
  and preferred x = x in
  { init; handler; preferred }
;;

let constant_pow c : ('env, dag_data) reward_function = fun ~view:_ ~assign -> assign c

let constant_block c : ('env, dag_data) reward_function =
 fun ~view:v ~assign n -> if v.data n |> is_block then assign c n
;;

let reward_functions =
  let open Collection in
  empty
  |> add ~info:"1 per confirmed block" "block" (constant_block 1.)
  |> add ~info:"1 per confirmed pow solution" "constant" (constant_pow 1.)
;;

module PrivateAttack = struct
  type 'env state =
    { public : 'env Dag.vertex (* defender's preferred block *)
    ; private_ : 'env Dag.vertex (* attacker's preferred block *)
    ; common : 'env Dag.vertex (* common chain *)
    ; use_defender_votes : bool
          (* if this flag is set to true, the attacker considers the defender's votes that
             extend on his preferred block when building a new block.

             If this flag is set to false, the attacker prolongs the current epoch until
             he can form a block that does not reference any defender votes. *)
    }

  (* call this whenever public or private_ changes *)
  let set_common (v : _ local_view) state =
    let common = Dag.common_ancestor v.view state.public state.private_ in
    assert (Option.is_some common) (* all our protocols maintain this invariant *);
    { state with common = Option.get common }
  ;;

  (* the attacker emulates a defending node. This is the local_view of the defender *)
  let public_view _state (v : _ local_view) =
    let visible = v.released in
    { v with
      view = Dag.filter visible v.view
    ; appended_by_me =
        (* The attacker simulates an honest node on the public view. This node should not
           interpret attacker vertices as own vertices. *)
        (fun _ -> false)
    }
    |> extend_view
  ;;

  (* the attacker emulates a defending node. This describes the defender node *)
  let handle_public ~k v actions state event =
    let view = public_view state v
    and drop_messages = { actions with share = (fun _n -> ()) } in
    let public = honest_handler ~k view drop_messages state.public event in
    set_common v { state with public }
  ;;

  (* the attacker works on a subset of the total information: he ignores new defender
     blocks *)
  let private_view state (v : _ local_view) =
    let visible vertex =
      (state.use_defender_votes
      && v.data vertex |> is_vote
      && last_block (extend_view v) vertex $== state.private_)
      (* defender votes for the attacker's preferred block *)
      || v.appended_by_me vertex
      (* anything mined by the attacker *)
      || Dag.partial_order state.common vertex >= 0
      (* anything on the common chain *)
    in
    { v with view = Dag.filter visible v.view } |> extend_view
  ;;

  let handle_private ~k v actions state event =
    let view = private_view state v
    and drop_messages = { actions with share = (fun _n -> ()) } in
    let private_ = honest_handler ~k view drop_messages state.private_ event in
    set_common v { state with private_ }
  ;;

  let handler ~k v actions state event =
    match event with
    | Activate _ ->
      (* work on private chain *)
      handle_private ~k v actions state event
    | Deliver _ ->
      (* simulate defender *)
      handle_public ~k v actions state event
  ;;

  (* passive node w/o tactic *)
  let withhold ~k (v : _ local_view) =
    let handler = handler ~k v
    and preferred x = x.private_
    and init ~roots =
      let v = (honest ~k v).init ~roots in
      { public = v; private_ = v; common = v; use_defender_votes = true }
    in
    { init; handler; preferred }
  ;;

  type 'env action =
    { release : 'env Dag.vertex list (* release these vertices and all dependencies *)
    ; adopt : bool (* adopt public chain and discard private blocks *)
    ; use_defender_votes : bool (* control mining behaviour *)
    }

  type ('env, 'dag_data) tactic =
    ('env, 'dag_data) local_view -> 'env state -> 'env action

  (* release a given vertex and all its dependencies recursively *)
  let rec release_recursive v release ns =
    List.iter
      (fun n ->
        if not (v.released n)
        then (
          release n;
          release_recursive v release (Dag.parents v.view n)))
      ns
  ;;

  let apply_tactic ~k (tactic : ('env, 'dag_data) tactic) (v : _ local_view) actions state
    =
    let { release; adopt; use_defender_votes } = tactic v state in
    let state =
      (* release information and simulate defender *)
      let () = release_recursive (extend_view v) actions.share release in
      List.fold_left
        (fun acc el -> handle_public ~k v actions acc (Deliver el))
        state
        release
    in
    (* adopt defender chain (or don't) *)
    if adopt
    then { state with private_ = state.public; common = state.public; use_defender_votes }
    else state
  ;;

  let node ~k (tactic : ('env, 'dag_data) tactic) (v : _ local_view) =
    let node = withhold ~k v in
    let handler actions state event =
      node.handler actions state event |> apply_tactic ~k tactic v actions
    in
    { node with handler }
  ;;

  module Observation = struct
    type t =
      { public_blocks : int (** number of blocks after common ancestor *)
      ; public_votes : int (** number of votes confirming the leading block *)
      ; private_blocks : int (** number of blocks after common ancestor *)
      ; private_votes : int (** number of votes confirming the leading block *)
      ; diff_blocks : int (** private_blocks - public_blocks *)
      ; diff_votes : int (** private_votes - public_votes *)
      }
    [@@deriving fields]

    let length = List.length Fields.names

    let observe v s =
      let private_view = extend_view v
      and public_view = public_view s v in
      let private_votes = Dag.children private_view.votes_only s.private_ |> List.length
      and public_votes = Dag.children public_view.votes_only s.public |> List.length in
      let v = private_view in
      let ca =
        Dag.common_ancestor v.view s.private_ s.public |> Option.get |> last_block v
      in
      let ca_height = block_height_exn v ca
      and private_height = block_height_exn v s.private_
      and public_height = block_height_exn v s.public in
      { private_blocks = private_height - ca_height
      ; public_blocks = public_height - ca_height
      ; diff_blocks = private_height - public_height
      ; private_votes
      ; public_votes
      ; diff_votes = private_votes - public_votes
      }
    ;;

    let low =
      { public_blocks = 0
      ; public_votes = 0
      ; private_blocks = 0
      ; private_votes = 0
      ; diff_blocks = min_int
      ; diff_votes = min_int
      }
    ;;

    let high =
      { public_blocks = max_int
      ; public_votes = max_int
      ; private_blocks = max_int
      ; private_votes = max_int
      ; diff_blocks = max_int
      ; diff_votes = max_int
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
          ~public_votes:int
          ~private_blocks:int
          ~private_votes:int
          ~diff_blocks:int
          ~diff_votes:int
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
           ~public_votes:int
           ~private_blocks:int
           ~private_votes:int
           ~diff_blocks:int
           ~diff_votes:int)
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
        ~public_votes:int
        ~private_blocks:int
        ~private_votes:int
        ~diff_blocks:int
        ~diff_votes:int
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
          }
        in
        t = (to_floatarray t |> of_floatarray)
      in
      List.init 50 run |> List.for_all (fun x -> x)
    ;;
  end

  module Action = struct
    type t =
      | Adopt_Prolong
      | Override_Prolong
      | Match_Prolong
      | Wait_Prolong
      | Adopt_Proceed
      | Override_Proceed
      | Match_Proceed
      | Wait_Proceed
    [@@deriving variants]

    let to_string = Variants.to_name
    let to_int = Variants.to_rank

    let table =
      let add acc var = var.Variantslib.Variant.constructor :: acc in
      Variants.fold
        ~init:[]
        ~adopt_prolong:add
        ~override_prolong:add
        ~match_prolong:add
        ~wait_prolong:add
        ~adopt_proceed:add
        ~override_proceed:add
        ~match_proceed:add
        ~wait_proceed:add
      |> List.rev
      |> Array.of_list
    ;;

    let of_int i = table.(i)
    let n = Array.length table
  end

  let honest_policy o =
    let open Observation in
    let open Action in
    if o.private_blocks < o.public_blocks then Adopt_Proceed else Override_Proceed
  ;;

  let selfish_policy o =
    (* Ad-hoc strategy. This is probably not optimal. *)
    let open Observation in
    let open Action in
    if o.private_blocks < o.public_blocks
    then Adopt_Proceed
    else if o.private_blocks = 0 && o.public_blocks = 0
    then Wait_Prolong
    else if o.public_blocks = 0
    then Wait_Proceed
    else Override_Proceed
  ;;

  let policies = [ "honest", honest_policy; "selfish", selfish_policy ]

  let apply_action ~k v state action =
    let parent_block v n =
      match Dag.parents v.view n with
      | hd :: _ when v.data hd |> is_block -> Some hd
      | _ -> None
    in
    let match_ offset =
      let height, nvotes =
        let v = public_view state v in
        let target =
          (block_height_exn v state.public * k)
          + List.length (Dag.children v.votes_only state.public)
          + offset
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
      match first Compare.(by float v.delivered_at) nvotes votes with
      | Some subset -> block :: subset
      | None ->
        (* not enough votes, release all *)
        block :: votes
    in
    let use_defender_votes =
      match (action : Action.t) with
      | Adopt_Proceed | Match_Proceed | Override_Proceed | Wait_Proceed -> true
      | Adopt_Prolong | Match_Prolong | Override_Prolong | Wait_Prolong -> false
    in
    match (action : Action.t) with
    | Adopt_Proceed | Adopt_Prolong -> { release = []; adopt = true; use_defender_votes }
    | Match_Proceed | Match_Prolong ->
      { release = match_ 0; adopt = false; use_defender_votes }
    | Override_Proceed | Override_Prolong ->
      { release = match_ 1; adopt = false; use_defender_votes }
    | Wait_Proceed | Wait_Prolong -> { release = []; adopt = false; use_defender_votes }
  ;;

  let lift_policy p (v : _ local_view) state : Action.t = Observation.observe v state |> p
  let tactic_of_policy ~k p v state = (lift_policy p) v state |> apply_action ~k v state
  let tactic_of_policy' ~k p v state = p v state |> apply_action ~k v state

  let attack ~k p = Node (node ~k (tactic_of_policy ~k p))
  and attack' ~k p = Node (node ~k (tactic_of_policy' ~k p))
end

let attacks ~k =
  let open Collection in
  empty
  |> add
       ~info:"Private attack with honest policy"
       "private-honest"
       PrivateAttack.(attack ~k honest_policy)
  |> add
       ~info:"Private attack with selfish policy"
       "private-selfish"
       PrivateAttack.(attack ~k selfish_policy)
;;

let protocol ~k =
  { key = "bk+ll"
  ; info = "Bₖ with less leader modification with k=" ^ string_of_int k
  ; pow_per_block = k
  ; honest = honest ~k
  ; dag_validity = dag_validity ~k
  ; dag_roots
  ; describe
  ; height
  ; reward_functions
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
      (match (Dag.data n).value with
      | Block b ->
        if b.height < height
        then (
          Printf.eprintf "k: %i\theight: %i\texpected: %i\n" k b.height height;
          false)
        else true (* more than 900 blocks in a sequence imply less than 10% orphans. *)
      | _ -> failwith "invalid dag")
  in
  List.for_all
    test
    [ 08, 10., 900 (* good condition, 10% orphans *)
    ; 08, 01., 700 (* bad conditions, 30% orphans *)
    ; 32, 01., 900
      (* bad conditions, 10% orphans, high k *)
    ]
;;
