open Cpr_lib
open Protocol

type block = { height : int }

type node =
  | Vote
  | Block of block

let is_vote = function
  | Vote -> true
  | _ -> false
;;

let is_block = function
  | Block _ -> true
  | _ -> false
;;

(* TODO verify vote uniqueness *)
let dag_validity ~k (v : _ global_view) n =
  match v.pow_hash n, v.data n, Dag.parents v.view n with
  | None, _, _ -> false
  | Some _, Vote, [ p ] -> v.data p |> is_block
  | Some _, Block b', hd :: tl ->
    (match v.data hd with
    | Block b ->
      List.for_all (fun n -> v.data n |> is_vote) tl
      && List.length tl = k - 1
      && b.height + 1 = b'.height
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
  ; appended_by_me : 'env Dag.node -> bool
  ; released : 'env Dag.node -> bool
  ; my_id : int
  }

let extend_view (x : _ Protocol.local_view) =
  { view = x.view
  ; data = x.data
  ; votes_only = Dag.filter (fun n -> x.data n |> is_vote) x.view
  ; blocks_only = Dag.filter (fun n -> x.data n |> is_block) x.view
  ; delivered_at = x.delivered_at
  ; appended_by_me = x.appended_by_me
  ; released = x.released
  ; my_id = x.my_id
  }
;;

let last_block v n =
  match v.data n with
  | Block _ -> n
  | Vote ->
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

let first by n l =
  let a = Array.of_list l in
  if Array.length a < n then raise (Invalid_argument "list too short");
  let () = Array.sort (fun a b -> compare (by a) (by b)) a in
  let l = ref [] in
  for i = 0 to n - 1 do
    l := a.(i) :: !l
  done;
  !l
;;

let compare_blocks v =
  let open Compare in
  let cmp =
    by int (block_height_exn v)
    $ by int (fun n -> List.length (Dag.children v.votes_only n))
    $ by int (fun n -> if v.appended_by_me n then 1 else 0)
    $ by (inv float) v.delivered_at
  in
  skip_eq Dag.node_eq cmp
;;

let update_head v ~preferred ~consider =
  if compare_blocks v consider preferred > 0 then consider else preferred
;;

let honest ~k v actions preferred = function
  | Activate pow ->
    let votes = Dag.children v.votes_only preferred in
    if List.length votes >= k - 1
    then (
      let head = block_data_exn v preferred in
      let head' =
        actions.extend_dag
          ~pow
          (preferred
          :: first
               (fun n -> (if v.appended_by_me n then 0 else 1), v.delivered_at n)
               (k - 1)
               votes)
          (Block { height = head.height + 1 })
      in
      actions.share head';
      head')
    else (
      let vote = actions.extend_dag ~pow [ preferred ] Vote in
      actions.share vote;
      preferred)
  | Deliver n ->
    (* We only prefer blocks. For received votes, reconsider parent block. *)
    let consider = last_block v n in
    update_head v ~preferred ~consider
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

let constant_pow c : ('env, node) reward_function = fun ~view:_ ~assign -> assign c

let constant_block c : ('env, node) reward_function =
 fun ~view:v ~assign n -> if v.data n |> is_block then assign c n
;;

type 'a strategic_state =
  { public : 'a Dag.node
  ; private_ : 'a Dag.node
  }

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

let honest_tactic v actions state withheld =
  List.iter actions.share withheld;
  update_head v ~preferred:state.private_ ~consider:state.public
;;

(* Withhold until I can propose a block. George calls this proof-packing. *)
let simple_tactic v actions state _withheld =
  let privh = block_height_exn v state.private_
  and publh = block_height_exn v state.public in
  if publh > privh
  then (* abort withholding *)
    state.public
  else if privh > publh
  then (
    (* overwrite public chain *)
    release v actions [ state.private_ ];
    state.private_)
  else (* continue withholding *)
    state.private_
;;

(* Withhold until defender proposes a block then overwrite. George calls this long-range. *)
(* TODO fix for k=1 *)
let advanced_tactic v actions state _withheld =
  if state.private_ $== state.public
  then state.private_
  else (
    let d = compare_blocks v state.private_ state.public in
    if d < 0
    then (* Falling behind. Abort withholding *)
      state.public
    else if d > 0
    then (
      (* Overwrite feasible. *)
      let npubv = Dag.children v.votes_only state.public |> List.length in
      let releasehead =
        (* TODO it may be easier to find common ancestor and release downwards until the
           preference of the defender changes *)
        (* release least number of blocks ... *)
        let rec f b =
          if block_height_exn v b > block_height_exn v state.public
          then (
            match Dag.parents v.blocks_only b with
            | [ p ] -> f p
            | _ -> failwith "invalid DAG")
          else b
        in
        f state.private_
      in
      (* ... and least number of votes *)
      let privv = Dag.children v.votes_only releasehead in
      if List.length privv > npubv
      then release v actions (releasehead :: first v.delivered_at (npubv + 1) privv);
      state.private_)
    else state.private_)
;;

let strategic tactic ~k (v : _ local_view) =
  let public_view = extend_view { v with view = Dag.filter v.released v.view }
  and private_view = extend_view v in
  let tactic = tactic private_view in
  let handler actions state event =
    let withheld = ref [] in
    let withhold = { actions with share = (fun n -> withheld := n :: !withheld) } in
    let state =
      match event with
      | Activate _ ->
        { state with private_ = honest ~k private_view withhold state.private_ event }
      | Deliver _ ->
        { state with public = honest ~k public_view withhold state.public event }
    in
    let withheld = List.rev !withheld in
    { state with private_ = tactic actions state withheld }
  and preferred x = x.private_
  and init ~roots =
    let s = init ~roots in
    { public = s; private_ = s }
  in
  { init; handler; preferred }
;;
