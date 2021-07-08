open Cpr_lib
open Protocol

type block = { height : int }

type node =
  | Vote
  | Block of block

let dag_invariant ~k ~pow parents child =
  match pow, parents, child with
  | true, [ Block _ ], Vote -> true
  | true, Block b :: votes, Block b' ->
    List.for_all
      (function
        | Vote -> true
        | _ -> false)
      votes
    && List.length votes = k - 1
    && b.height + 1 = b'.height
  | _ -> false
;;

let dag_roots = [ Block { height = 0 } ]

let init ~roots =
  match roots with
  | [ genesis ] -> genesis
  | _ -> failwith "invalid roots"
;;

let is_vote = function
  | Vote -> true
  | _ -> false
;;

let is_block = function
  | Block _ -> true
  | _ -> false
;;

let last_block ctx gnode =
  match Dag.data gnode |> ctx.read with
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

let first n =
  let rec h n acc l =
    if n <= 0
    then List.rev acc
    else (
      match l with
      | [] -> raise (Invalid_argument "list too short")
      | hd :: tl -> h (n - 1) (hd :: acc) tl)
  in
  h n []
;;

let honest ~k ctx =
  let votes_only = Dag.filter (fun n -> ctx.read n |> is_vote) ctx.view
  and blocks_only = Dag.filter (fun n -> ctx.read n |> is_block) ctx.view
  and data n = Dag.data n |> ctx.read in
  let handler actions preferred = function
    | Activate pow ->
      let votes = Dag.children votes_only preferred in
      if List.length votes >= k - 1
      then (
        let head = block_data_exn data preferred in
        let head' =
          actions.extend_dag
            ~pow
            (preferred :: first (k - 1) votes)
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
        if Dag.have_common_ancestor blocks_only gblock preferred
        then (
          let consider gpref gblock =
            let pref = block_data_exn data gpref
            and block = block_data_exn data gblock in
            if block.height > pref.height
               || (block.height = pref.height
                  && List.length (Dag.children votes_only gblock)
                     > List.length (Dag.children votes_only gpref))
            then gblock
            else gpref
          in
          (* delayed block might connect nodes delivered previously *)
          let preferred =
            List.fold_left consider preferred (Dag.leaves blocks_only gblock)
          in
          assert (is_block (Dag.data preferred |> ctx.read));
          preferred)
        else preferred)
  and preferred x = x in
  Node { init; handler; preferred }
;;

let protocol ~k = { honest = honest ~k; dag_invariant = dag_invariant ~k; dag_roots }

let%test "convergence" =
  let open Simulator in
  let test k params height =
    init params (protocol ~k)
    |> loop params
    |> fun { nodes; global_view; _ } ->
    Array.to_seq nodes
    |> Seq.map (fun (SNode x) -> x.preferred x.state)
    |> Dag.common_ancestor' (Dag.filter (fun x -> is_block x.value) global_view)
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
 fun ctx reward head ->
  let blocks = Dag.filter (fun x -> ctx.read x |> is_block) ctx.view
  and votes = Dag.filter (fun x -> ctx.read x |> is_vote) ctx.view in
  Seq.iter
    (fun b ->
      reward c b;
      List.iter (reward c) (Dag.parents votes b))
    (Dag.seq_history blocks head)
;;

type 'a selfish_state =
  { preferred : 'a Dag.node
  ; withheld : 'a Dag.node list
  }

let selfish ~k ctx =
  (* TODO: implement withholding *)
  let votes_only = Dag.filter (fun n -> ctx.read n |> is_vote) ctx.view
  and blocks_only = Dag.filter (fun n -> ctx.read n |> is_block) ctx.view
  and data n = Dag.data n |> ctx.read in
  let handler actions state = function
    | Activate pow ->
      let votes = Dag.children votes_only state.preferred in
      if List.length votes >= k - 1
      then (
        let head = block_data_exn data state.preferred in
        let head' =
          actions.extend_dag
            ~pow
            (state.preferred :: first (k - 1) votes)
            (Block { height = head.height + 1 })
        in
        actions.share head';
        { state with preferred = head' })
      else (
        let vote = actions.extend_dag ~pow [ state.preferred ] Vote in
        actions.share vote;
        state)
    | Deliver gnode ->
      (* We only prefer blocks. For received votes, reconsider parent block. *)
      (match last_block ctx gnode with
      | None -> state
      | Some gblock ->
        (* Only consider block if its heritage is visible. *)
        if Dag.have_common_ancestor blocks_only gblock state.preferred
        then (
          let consider gpref gblock =
            let pref = block_data_exn data gpref
            and block = block_data_exn data gblock in
            if block.height > pref.height
               || (block.height = pref.height
                  && List.length (Dag.children votes_only gblock)
                     > List.length (Dag.children votes_only gpref))
            then gblock
            else gpref
          in
          (* delayed block might connect nodes delivered previously *)
          let preferred =
            List.fold_left consider state.preferred (Dag.leaves blocks_only gblock)
          in
          assert (is_block (Dag.data preferred |> ctx.read));
          { state with preferred })
        else state)
  and preferred x = x.preferred
  and init ~roots = { preferred = init ~roots; withheld = [] } in
  Node { init; handler; preferred }
;;
