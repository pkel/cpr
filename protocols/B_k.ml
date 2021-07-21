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
  ; pow_hash : 'env Dag.node -> int option
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

let parent_block v n =
  match Dag.parents v.view n with
  | hd :: _ when v.data hd |> is_block -> Some hd
  | _ -> None
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
  | _ -> raise (Invalid_argument "invalid dag / block")
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

let preference v ~preferred ~consider =
  if Dag.node_eq preferred consider
  then preferred
  else (
    let pheight = block_height_exn v preferred
    and cheight = block_height_exn v consider in
    if cheight > pheight
    then consider
    else if cheight = pheight
    then (
      let cvotes = List.length (Dag.children v.votes_only consider)
      and pvotes = List.length (Dag.children v.votes_only preferred) in
      if cvotes > pvotes
      then consider
      else if cvotes = pvotes && leader_hash_exn v consider < leader_hash_exn v preferred
      then consider
      else preferred)
    else preferred)
;;

let honest ~k v actions =
  let propose b =
    let pow_hash_exn n = v.pow_hash n |> Option.get in
    first ~skip_to:v.appended_by_me pow_hash_exn k (Dag.children v.votes_only b)
    |> Option.map (fun q ->
           (* only propose if there is no better proposal *)
           let this = List.hd q |> pow_hash_exn in
           if List.for_all
                (fun n -> leader_hash_exn v n > this)
                (Dag.children v.blocks_only b)
           then (
             let block =
               actions.extend_dag
                 ~sign:true
                 (b :: q)
                 (Block { height = block_height_exn v b + 1 })
             in
             actions.share block;
             Some block)
           else None)
    |> Option.join
  in
  fun preferred -> function
    | Activate pow ->
      let vote = actions.extend_dag ~pow [ preferred ] (Vote v.my_id) in
      actions.share vote;
      Option.value ~default:preferred (propose preferred)
    | Deliver n ->
      (* We only prefer blocks. For received votes, reconsider parent block. *)
      let consider = last_block v n in
      (match propose consider with
      | None -> preference v ~preferred ~consider
      | Some consider -> preference v ~preferred ~consider)
;;

let protocol ~k =
  let honest v =
    let handler = honest ~k (extend_view v)
    and preferred x = x in
    Node { init; handler; preferred }
  in
  { honest; dag_validity = dag_validity ~k; dag_roots }
;;

let%test "convergence" =
  let open Simulator in
  let test k params height =
    init params (protocol ~k)
    |> loop params
    |> fun { nodes; global; _ } ->
    Array.to_seq nodes
    |> Seq.map (fun (SNode x) -> x.preferred x.state)
    |> Dag.common_ancestor'
         (Dag.filter (fun x -> is_block (Dag.data x).value) global.view)
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

let constant c : ('env, node) reward_function =
 fun ~view:v ~assign n ->
  match v.pow_hash n with
  | Some _ -> assign c n
  | None -> ()
;;

type 'env strategic_state =
  { public : 'env Dag.node
  ; private_ : 'env Dag.node
  }

(* release a given node and all it's dependencies recursively *)
let release v actions n =
  (* TODO make recursive and stop iteration of first released block *)
  Dag.iterate_ancestors v.view [ n ]
  |> Seq.iter (fun n -> if not (v.released n) then actions.share n)
;;

let honest_tactic v actions state withheld =
  List.iter actions.share withheld;
  preference v ~preferred:state.private_ ~consider:state.public
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
    release v actions state.private_;
    state.private_)
  else (* continue withholding *)
    state.private_
;;

let strategic tactic ~k (v : _ local_view) =
  let public_view = extend_view { v with view = Dag.filter v.released v.view }
  and private_view = extend_view v in
  let tactic =
    match tactic with
    | `Honest -> honest_tactic private_view
    | `Simple -> simple_tactic private_view
  in
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
  Node { init; handler; preferred }
;;
