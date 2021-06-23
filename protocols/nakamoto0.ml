open Cpr_lib

type data = { height : int }

let dag_roots = [ { height = 0 } ]

let dag_invariant ~pow parents child =
  match pow, parents with
  | true, [ p ] -> child.height = p.height + 1
  | _ -> false
;;

type 'a state = 'a Dag.node

module Spawn (CTX : Protocol_intf.CONTEXT with type data := data) = struct
  open CTX

  let init ~roots =
    match roots with
    | [ genesis ] -> genesis
    | _ -> failwith "invalid roots"
  ;;

  let data n = Dag.data n |> read
  let parents = Dag.parents view
  let children = Dag.children view

  let have_common_ancestor =
    let rec h a b =
      if a == b
      then true
      else (
        let a' = data a
        and b' = data b in
        if a'.height = b'.height
        then (
          match parents a, parents b with
          | [ a ], [ b ] -> h a b
          | [], _ | _, [] -> false
          | _ -> failwith "invalid dag")
        else if a'.height > b'.height
        then (
          match parents a with
          | [ a ] -> h a b
          | [] -> false
          | _ -> failwith "invalid dag")
        else (
          match parents b with
          | [ b ] -> h a b
          | [] -> false
          | _ -> failwith "invalid dag"))
    in
    h
  ;;

  let leaves gnode =
    let rec h acc gnode =
      match children gnode with
      | [] -> gnode :: acc
      | l -> List.fold_left h acc l
    in
    h [] gnode
  ;;

  let handler preferred = function
    | Activate pow ->
      let head = data preferred in
      let head' = extend_dag ~pow [ preferred ] { height = head.height + 1 } in
      share head';
      head'
    | Deliver gnode ->
      (* Only consider gnode if its heritage is visible. *)
      if have_common_ancestor gnode preferred
      then (
        let consider preferred gnode =
          let node = data gnode
          and head = data preferred in
          if node.height > head.height then gnode else preferred
        in
        (* delayed gnode might connect nodes delivered previously *)
        List.fold_left consider preferred (leaves gnode))
      else preferred
  ;;
end

(*
  let%test _ =
    let open Simulator in
    let params =
      { n_nodes = 32; n_activations = 1000; activation_delay = 10.; message_delay = 1. }
    in
    init params protocol
    |> loop params
    |> fun { node_state; _ } ->
    Array.for_all
      (fun pref ->
         (Dag.data pref).value.height > 900
         (* more than 900 blocks in a sequence imply less than 10% orphans. *))
      node_state
  ;;
   *)
