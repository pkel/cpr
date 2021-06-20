open Cpr_lib
open Protocol

type block = { height : int }

let dag_roots = [ { height = 0 } ]

let dag_invariant ~pow ~parents ~child =
  match pow, parents with
  | true, [ p ] -> child.height = p.height + 1
  | _ -> false
;;

let init ~roots =
  match roots with
  | [ genesis ] -> genesis
  | _ -> failwith "invalid roots"
;;

let have_common_ancestor view =
  let rec h a b =
    if a == b
    then true
    else (
      let a' = Dag.data view a
      and b' = Dag.data view b in
      if a'.height = b'.height
      then (
        match Dag.parents view a, Dag.parents view b with
        | [ a ], [ b ] -> h a b
        | [], _ | _, [] -> false
        | _ -> failwith "invalid dag")
      else if a'.height > b'.height
      then (
        match Dag.parents view a with
        | [ a ] -> h a b
        | [] -> false
        | _ -> failwith "invalid dag")
      else (
        match Dag.parents view b with
        | [ b ] -> h a b
        | [] -> false
        | _ -> failwith "invalid dag"))
  in
  h
;;

let leaves view gnode =
  let rec h acc gnode =
    match Dag.children view gnode with
    | [] -> gnode :: acc
    | l -> List.fold_left h acc l
  in
  h [] gnode
;;

let event_handler ctx preferred = function
  | Activate pow ->
    let head = Dag.data ctx.view preferred in
    let head' = ctx.extend_dag ~pow [ preferred ] { height = head.height + 1 } in
    ctx.release head';
    head'
  | Deliver gnode ->
    (* Only consider gnode if its heritage is visible. *)
    if have_common_ancestor ctx.view gnode preferred
    then (
      let consider preferred gnode =
        let node = Dag.data ctx.view gnode
        and head = Dag.data ctx.view preferred in
        if node.height > head.height then gnode else preferred
      in
      (* delayed gnode might connect nodes delivered previously *)
      List.fold_left consider preferred (leaves ctx.view gnode))
    else preferred
;;

let protocol : _ protocol = { init; dag_roots; dag_invariant; event_handler }

let%expect_test _ =
  let open Simulator in
  let params =
    { n_nodes = 32; n_activations = 1000; activation_delay = 1.; message_delay = 1. }
  in
  init params protocol |> loop params |> ignore
;;
