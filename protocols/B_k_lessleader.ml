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
let dag_validity ~k ~pow ~view ~read n =
  let data n = Dag.data n |> read in
  match pow, data n, Dag.parents view n with
  | false, _, _ -> false
  | true, Vote, [ p ] -> data p |> is_block
  | true, Block b', hd :: tl ->
    (match data hd with
    | Block b ->
      List.for_all (fun n -> data n |> is_vote) tl
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

type ('env, 'data) extended_context =
  { view : 'env Dag.view
  ; data : 'env Dag.node -> 'data
  ; votes_only : 'env Dag.view
  ; blocks_only : 'env Dag.view
  ; received_at : 'env Dag.node -> float
  ; mined_myself : 'env Dag.node -> bool
  }

let extend_ctx (x : _ Protocol.context) =
  let data n = Dag.data n |> x.read
  and mined_myself n = Dag.data n |> x.mined_myself
  and received_at n = Dag.data n |> x.received_at in
  { view = x.view
  ; data
  ; votes_only = Dag.filter (fun n -> data n |> is_vote) x.view
  ; blocks_only = Dag.filter (fun n -> data n |> is_block) x.view
  ; received_at
  ; mined_myself
  }
;;

let last_block ctx gnode =
  match ctx.data gnode with
  | Block _ -> Some gnode
  | Vote ->
    (match Dag.parents ctx.view gnode with
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

let honest ~k ctx =
  let ctx = extend_ctx ctx in
  let handler actions preferred = function
    | Activate pow ->
      let votes = Dag.children ctx.votes_only preferred in
      if List.length votes >= k - 1
      then (
        let head = block_data_exn ctx.data preferred in
        let head' =
          actions.extend_dag
            ~pow
            (preferred :: first ctx.received_at (k - 1) votes)
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
      (match last_block ctx gnode with
      | None -> preferred
      | Some gblock ->
        (* Only consider block if its heritage is visible. *)
        if Dag.have_common_ancestor ctx.blocks_only gblock preferred
        then (
          let consider gpref gblock =
            let pref = block_data_exn ctx.data gpref
            and block = block_data_exn ctx.data gblock in
            if block.height > pref.height
               || (block.height = pref.height
                  && List.length (Dag.children ctx.votes_only gblock)
                     > List.length (Dag.children ctx.votes_only gpref))
            then gblock
            else gpref
          in
          (* delayed block might connect nodes delivered previously *)
          let preferred =
            List.fold_left consider preferred (Dag.leaves ctx.blocks_only gblock)
          in
          assert (ctx.data preferred |> is_block);
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
    |> fun { nodes; global_view; _ } ->
    Array.to_seq nodes
    |> Seq.map (fun (SNode x) -> x.preferred x.state)
    |> Dag.common_ancestor'
         (Dag.filter (fun x -> is_block (Dag.data x).value) global_view)
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

let constant c : ('env, node) reward_function = fun ~view:_ ~read:_ ~assign -> assign c

type 'a selfish_state =
  { public_head : 'a Dag.node
  ; private_head : 'a Dag.node
  ; withheld : 'a Dag.node list
  }

let preference ctx ~preferred:gpref ~consider:gblock =
  let pref = block_data_exn ctx.data gpref
  and block = block_data_exn ctx.data gblock in
  if block.height > pref.height
     || (block.height = pref.height
        && List.length (Dag.children ctx.votes_only gblock)
           > List.length (Dag.children ctx.votes_only gpref))
  then gblock
  else gpref
;;

let strategy tactic ~k ctx actions state =
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
  let public = block_data_exn ctx.data state.public_head
  and privat = block_data_exn ctx.data state.private_head in
  match tactic with
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
          preference ctx ~preferred:state.private_head ~consider:state.public_head
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
              (preference ctx ~preferred:state.private_head ~consider:state.public_head)
    then
      (* we are falling behind; abort *)
      release state.withheld { state with private_head = state.public_head }
    else (
      (* overwrite public chain *)
      let npubv = Dag.children ctx.votes_only state.public_head |> List.length in
      let releasehead =
        (* release least number of blocks *)
        let rec f b =
          if (block_data_exn ctx.data b).height > public.height
          then (
            match Dag.parents ctx.blocks_only b with
            | [ p ] -> f p
            | _ -> failwith "invalid DAG")
          else b
        in
        f state.private_head
      in
      let privv = Dag.children ctx.votes_only releasehead in
      if List.length privv > npubv
      then (
        (* overwrite feasible *)
        let releaseheight = (block_data_exn ctx.data state.public_head).height in
        assert ((block_data_exn ctx.data releasehead).height = releaseheight);
        let blockdeps =
          List.filter
            (fun n ->
              match ctx.data n with
              | Block b -> b.height < releaseheight
              | Vote -> false)
            state.withheld
        in
        let releasenodes =
          List.concat
            [ [ releasehead ]
            ; first
                (fun n -> not (ctx.mined_myself n), ctx.received_at n)
                (npubv + 1)
                (Dag.children ctx.votes_only releasehead)
            ; Dag.parents ctx.votes_only releasehead
            ; blockdeps
            ; List.concat_map (Dag.parents ctx.votes_only) blockdeps
            ]
        in
        release releasenodes { state with public_head = releasehead })
      else (* overwrite infeasible *) state)
;;

let strategic tactic ~k ctx =
  let ctx = extend_ctx ctx in
  let handler actions state event =
    let state =
      match event with
      | Activate pow ->
        let votes = Dag.children ctx.votes_only state.private_head in
        if List.length votes >= k - 1
        then (
          let head = block_data_exn ctx.data state.private_head in
          let head' =
            actions.extend_dag
              ~pow
              (state.private_head
              :: first
                   (fun n -> not (ctx.mined_myself n), ctx.received_at n)
                   (k - 1)
                   votes)
              (Block { height = head.height + 1 })
          in
          { state with private_head = head'; withheld = head' :: state.withheld })
        else (
          let vote = actions.extend_dag ~pow [ state.private_head ] Vote in
          { state with withheld = vote :: state.withheld })
      | Deliver gnode ->
        (* simulate honest node *)
        let public n = List.exists (fun x -> Dag.node_eq x n) state.withheld |> not in
        let ctx =
          { ctx with
            view = Dag.filter public ctx.view
          ; votes_only = Dag.filter public ctx.votes_only
          ; blocks_only = Dag.filter public ctx.blocks_only
          }
        in
        (* We only prefer blocks. For received votes, reconsider parent block. *)
        (match last_block ctx gnode with
        | None -> state
        | Some gblock ->
          (* Only consider block if its heritage is visible. *)
          if Dag.have_common_ancestor ctx.blocks_only gblock state.public_head
          then (
            (* delayed block might connect nodes delivered previously *)
            let public_head =
              List.fold_left
                (fun preferred consider -> preference ctx ~preferred ~consider)
                state.public_head
                (Dag.leaves ctx.blocks_only gblock)
            in
            assert (is_block (ctx.data public_head));
            { state with public_head })
          else state)
    in
    strategy ~k tactic ctx actions state
  and preferred x = x.private_head
  and init ~roots =
    let genesis = init ~roots in
    { public_head = genesis; private_head = genesis; withheld = [] }
  in
  Node { init; handler; preferred }
;;
