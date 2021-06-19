open Cpr_lib
open Protocol

type block = { height : int }

type node =
  | Vote
  | Block of block

let dag_invariant ~k ~pow ~parents ~child =
  match pow, parents, child with
  | true, [ Block _ ], Vote -> true
  | true, Block b :: votes, Block b' ->
    List.for_all
      (function
        | Vote -> true
        | _ -> false)
      votes
    && List.length votes = k
    && b.height + 1 = b'.height
  | _ -> false
;;

let vote_children view block =
  Dag.children view block
  |> List.filter (fun node ->
         match Dag.data view node with
         | Vote -> true
         | Block _ -> false)
;;

let block_data_exn view node =
  match Dag.data view node with
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

let event_handler ~k ctx preferred = function
  | Activate pow ->
    let votes = vote_children ctx.view preferred in
    if List.length votes >= k
    then (
      let head = block_data_exn ctx.view preferred in
      let head' =
        ctx.extend_dag
          ~pow
          (preferred :: first k votes)
          (Block { height = head.height + 1 })
      in
      ctx.release head';
      head')
    else (
      let vote = ctx.extend_dag [ preferred ] Vote in
      ctx.release vote;
      preferred)
  | Deliver gnode ->
    (* TODO: Preference can break when order of messages is off. *)
    (* TODO: Only prefer gnode if its heritage is visible. *)
    (* TODO: Consider gnode's offspring. *)
    let head = block_data_exn ctx.view preferred in
    let update_head (gblock, block) =
      if block.height > head.height
         || (block.height = head.height
            && vote_children ctx.view gblock
               |> List.length
               > (vote_children ctx.view preferred |> List.length))
      then gblock
      else preferred
    in
    (match Dag.data ctx.view gnode with
    | Vote ->
      (match Dag.parents ctx.view gnode with
      | [] -> preferred (* parent not visible yet *)
      | [ gnode ] ->
        let node = block_data_exn ctx.view gnode in
        update_head (gnode, node)
      | _ -> failwith "invalid dag")
    | Block b -> update_head (gnode, b))
;;

let protocol ~k : _ protocol =
  { dag_invariant = dag_invariant ~k; event_handler = event_handler ~k }
;;
