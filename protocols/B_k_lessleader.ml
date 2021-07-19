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
  ; received_at : 'env Dag.node -> float
  ; appended_by_me : 'env Dag.node -> bool
  ; released : 'env Dag.node -> bool
  ; my_id : int
  }

let extend_view (x : _ Protocol.local_view) =
  { view = x.view
  ; data = x.data
  ; votes_only = Dag.filter (fun n -> x.data n |> is_vote) x.view
  ; blocks_only = Dag.filter (fun n -> x.data n |> is_block) x.view
  ; received_at = x.received_at
  ; appended_by_me = x.appended_by_me
  ; released = x.released
  ; my_id = x.my_id
  }
;;

let last_block v gnode =
  match v.data gnode with
  | Block _ -> Some gnode
  | Vote ->
    (match Dag.parents v.view gnode with
    | [] -> None
    | [ gnode ] -> Some gnode
    | _ -> failwith "invalid dag")
;;

let block_data_exn data node =
  match data node with
  | Block b -> b
  | _ -> raise (Invalid_argument "not a block")
;;

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

let honest ~k v =
  let v = extend_view v in
  let handler actions preferred = function
    | Activate pow ->
      let votes = Dag.children v.votes_only preferred in
      if List.length votes >= k - 1
      then (
        let head = block_data_exn v.data preferred in
        let head' =
          actions.extend_dag
            ~pow
            (preferred :: first v.received_at (k - 1) votes)
            (Block { height = head.height + 1 })
        in
        actions.share head';
        head')
      else (
        let vote = actions.extend_dag ~pow [ preferred ] Vote in
        actions.share vote;
        preferred)
    | Deliver gnode ->
      (* We only prefer blocks. For received votes, reconsider parent block. *)
      (match last_block v gnode with
      | None -> preferred
      | Some gblock ->
        (* Only consider block if its heritage is visible. *)
        if Dag.have_common_ancestor v.blocks_only gblock preferred
        then (
          let consider gpref gblock =
            let pref = block_data_exn v.data gpref
            and block = block_data_exn v.data gblock in
            if block.height > pref.height
               || (block.height = pref.height
                  && List.length (Dag.children v.votes_only gblock)
                     > List.length (Dag.children v.votes_only gpref))
            then gblock
            else gpref
          in
          (* delayed block might connect nodes delivered previously *)
          let preferred =
            List.fold_left consider preferred (Dag.leaves v.blocks_only gblock)
          in
          assert (v.data preferred |> is_block);
          preferred)
        else preferred)
  and preferred x = x in
  Node { init; handler; preferred }
;;

let protocol ~k = { honest = honest ~k; dag_validity = dag_validity ~k; dag_roots }

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

let constant c : ('env, node) reward_function = fun ~view:_ ~assign -> assign c

type 'a selfish_state =
  { public_head : 'a Dag.node
  ; private_head : 'a Dag.node
  ; withheld : 'a Dag.node list
  }

let preference v ~preferred:gpref ~consider:gblock =
  let pref = block_data_exn v.data gpref
  and block = block_data_exn v.data gblock in
  if block.height > pref.height
     || (block.height = pref.height
        && List.length (Dag.children v.votes_only gblock)
           > List.length (Dag.children v.votes_only gpref))
  then gblock
  else gpref
;;

let strategy tactic ~k v actions state =
  let release nodes state =
    let ht = Hashtbl.create k in
    List.iter (fun n -> Hashtbl.replace ht (Dag.id n) true) state.withheld;
    List.iter
      (fun n ->
        let id = Dag.id n in
        match Hashtbl.find_opt ht id with
        | Some true ->
          Hashtbl.replace ht id false;
          actions.share n
        | _ -> ())
      nodes;
    { state with
      withheld =
        List.filter
          (fun n ->
            match Hashtbl.find_opt ht (Dag.id n) with
            | Some true -> true
            | _ -> false)
          state.withheld
    }
  in
  let public = block_data_exn v.data state.public_head
  and privat = block_data_exn v.data state.private_head in
  match if k < 2 then `Honest else tactic with
  | `Honest ->
    let state =
      (* adopt freshly mined block, if any *)
      if privat.height > public.height
      then { state with public_head = state.private_head }
      else state
    in
    release
      state.withheld
      { state with
        private_head =
          preference v ~preferred:state.private_head ~consider:state.public_head
      }
  | `Simple ->
    (* withhold until I can propose block / George's proof-packing *)
    if public.height > privat.height
    then (* abort *)
      release state.withheld { state with private_head = state.public_head }
    else if public.height < privat.height
    then
      (* overwrite public chain *)
      release state.withheld { state with public_head = state.private_head }
    else (* continue withholding *) state
  | `Advanced ->
    (* withhold until defender can propose block / George's long-range *)
    if Dag.node_eq state.public_head state.private_head
    then (* continue *)
      state
    else if Dag.node_eq
              state.public_head
              (preference v ~preferred:state.private_head ~consider:state.public_head)
    then
      (* we are falling behind; abort *)
      release state.withheld { state with private_head = state.public_head }
    else (
      (* overwrite public chain *)
      let npubv = Dag.children v.votes_only state.public_head |> List.length in
      let releasehead =
        (* release least number of blocks *)
        let rec f b =
          if (block_data_exn v.data b).height > public.height
          then (
            match Dag.parents v.blocks_only b with
            | [ p ] -> f p
            | _ -> failwith "invalid DAG")
          else b
        in
        f state.private_head
      in
      let privv = Dag.children v.votes_only releasehead in
      if List.length privv > npubv
      then (
        (* overwrite feasible *)
        let releaseheight = (block_data_exn v.data state.public_head).height in
        assert ((block_data_exn v.data releasehead).height = releaseheight);
        let blockdeps =
          List.filter
            (fun n ->
              match v.data n with
              | Block b -> b.height < releaseheight
              | Vote -> false)
            state.withheld
        in
        let releasenodes =
          List.concat
            [ [ releasehead ]
            ; first
                (fun n -> not (v.appended_by_me n), v.received_at n)
                (npubv + 1)
                (Dag.children v.votes_only releasehead)
            ; Dag.parents v.votes_only releasehead
            ; blockdeps
            ; List.concat_map (Dag.parents v.votes_only) blockdeps
            ]
        in
        release releasenodes { state with public_head = releasehead })
      else (* overwrite infeasible *) state)
;;

let strategic tactic ~k v =
  let v = extend_view v in
  let handler actions state event =
    let state =
      match event with
      | Activate pow ->
        let votes = Dag.children v.votes_only state.private_head in
        if List.length votes >= k - 1
        then (
          let head = block_data_exn v.data state.private_head in
          let head' =
            actions.extend_dag
              ~pow
              (state.private_head
              :: first (fun n -> not (v.appended_by_me n), v.received_at n) (k - 1) votes
              )
              (Block { height = head.height + 1 })
          in
          { state with private_head = head'; withheld = head' :: state.withheld })
        else (
          let vote = actions.extend_dag ~pow [ state.private_head ] Vote in
          { state with withheld = vote :: state.withheld })
      | Deliver gnode ->
        (* simulate honest node *)
        let v =
          { v with
            view = Dag.filter v.released v.view
          ; votes_only = Dag.filter v.released v.votes_only
          ; blocks_only = Dag.filter v.released v.blocks_only
          }
        in
        (* We only prefer blocks. For received votes, reconsider parent block. *)
        (match last_block v gnode with
        | None -> state
        | Some gblock ->
          (* Only consider block if its heritage is visible. *)
          if Dag.have_common_ancestor v.blocks_only gblock state.public_head
          then (
            (* delayed block might connect nodes delivered previously *)
            let public_head =
              List.fold_left
                (fun preferred consider -> preference v ~preferred ~consider)
                state.public_head
                (Dag.leaves v.blocks_only gblock)
            in
            assert (is_block (v.data public_head));
            { state with public_head })
          else state)
    in
    strategy ~k tactic v actions state
  and preferred x = x.private_head
  and init ~roots =
    let genesis = init ~roots in
    { public_head = genesis; private_head = genesis; withheld = [] }
  in
  Node { init; handler; preferred }
;;
