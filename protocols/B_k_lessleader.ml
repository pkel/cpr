open Cpr_lib
open Protocol

type block = { height : int }

type node =
  | Vote
  | Block of block

let dag_roots = [ Block { height = 0 } ]

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

let init _ctx ~roots =
  match roots with
  | [ genesis ] -> genesis
  | _ -> failwith "invalid roots"
;;

let is_vote = function
  | Vote -> true
  | _ -> false
;;

let vote_children ctx block =
  let votes_only = Dag.filter (fun n -> ctx.read n |> is_vote) ctx.view in
  Dag.children votes_only block
;;

let block_data_exn ctx node =
  let data n = Dag.data n |> ctx.read in
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

let handler ~k ctx preferred =
  let data n = Dag.data n |> ctx.read in
  function
  | Activate pow ->
    let votes = vote_children ctx preferred in
    if List.length votes >= k - 1
    then (
      let head = block_data_exn ctx preferred in
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
    (* TODO: Preference can break when order of messages is off. *)
    (* TODO: Only prefer gnode if its heritage is visible. *)
    (* TODO: Consider gnode's offspring. *)
    let head = block_data_exn ctx preferred in
    let update_head (gblock, block) =
      if block.height > head.height
         || (block.height = head.height
            && vote_children ctx gblock
               |> List.length
               > (vote_children ctx preferred |> List.length))
      then gblock
      else preferred
    in
    (match data gnode with
    | Vote ->
      (match Dag.parents ctx.view gnode with
      | [] -> preferred (* parent not visible yet *)
      | [ gnode ] ->
        let node = block_data_exn ctx gnode in
        update_head (gnode, node)
      | _ -> failwith "invalid dag")
    | Block b -> update_head (gnode, b))
;;

let protocol ~k : _ protocol =
  { init; dag_roots; dag_invariant = dag_invariant ~k; handler = handler ~k }
;;

let%test _ =
  let open Simulator in
  let params =
    { n_nodes = 32; n_activations = 8000; activation_delay = 10.; message_delay = 1. }
  in
  init params (protocol ~k:8)
  |> loop params
  |> fun { node_state; _ } ->
  Array.for_all
    (fun pref ->
      match (Dag.data pref).value with
      | Block b -> b.height > 900
      (* more than 900 blocks in a sequence imply less than 10% orphans. *)
      | _ -> failwith "invalid dag")
    node_state
;;
