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

let spawn ~k ctx =
  let votes_only = Dag.filter (fun n -> ctx.read n |> is_vote) ctx.view
  and blocks_only = Dag.filter (fun n -> ctx.read n |> is_block) ctx.view
  and data n = Dag.data n |> ctx.read in
  let handler preferred = function
    | Activate pow ->
      let votes = Dag.children votes_only preferred in
      if List.length votes >= k - 1
      then (
        let head = block_data_exn data preferred in
        let head' =
          ctx.extend_dag
            ~pow
            (preferred :: first (k - 1) votes)
            (Block { height = head.height + 1 })
        in
        ctx.share head';
        head')
      else (
        let vote = ctx.extend_dag ~pow [ preferred ] Vote in
        ctx.share vote;
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
          List.fold_left consider preferred (Dag.leaves blocks_only gblock))
        else preferred)
  in
  { init; handler }
;;

let protocol ~k = { spawn = spawn ~k; dag_invariant = dag_invariant ~k; dag_roots }

let%test _ =
  let open Simulator in
  let params =
    { n_nodes = 32; n_activations = 8000; activation_delay = 10.; message_delay = 1. }
  in
  init params (protocol ~k:8)
  |> loop params
  |> fun { nodes; global_view; _ } ->
  Array.to_seq nodes
  |> Seq.map (fun x -> x.state)
  |> Dag.common_ancestor' (Dag.filter (fun x -> is_block x.value) global_view)
  |> function
  | None -> false
  | Some n ->
    (match (Dag.data n).value with
    | Block b ->
      b.height > 900 (* more than 900 blocks in a sequence imply less than 10% orphans. *)
    | _ -> failwith "invalid dag")
;;
