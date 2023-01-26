open Cpr_lib

let key = "nakamoto"
let description = "Nakamoto consensus"

let info =
  let open Info in
  [ string "family" "nakamoto" ]
;;

type data =
  { height : int
  ; miner : int option
  }

let roots = [ { height = 0; miner = None } ]
let height x = x.height

module Referee (D : BlockDAG with type data = data) = struct
  include D

  let info x =
    let x = data x in
    let open Info in
    [ int "height" x.height
    ; Option.map string_of_int x.miner |> Option.value ~default:"n/a" |> string "miner"
    ]
  ;;

  let label x = Printf.sprintf "block %i" (data x).height

  let validity vertex =
    match pow vertex, parents vertex with
    | Some _, [ p ] ->
      let child = data vertex
      and p = data p in
      child.height = p.height + 1 && Option.is_some child.miner
    | _ -> false
  ;;

  let progress x = float_of_int (data x).height

  let winner = function
    | [] -> failwith "nakamoto.winner: empty list"
    | hd :: tl ->
      let height x = (data x).height in
      List.fold_left (fun acc x -> if height x > height acc then x else acc) hd tl
  ;;

  let precursor this = List.nth_opt (parents this) 0

  let reward v =
    match (data v).miner with
    | Some i -> [ i, 1. ]
    | None -> []
  ;;
end

let referee (type a) (module D : BlockDAG with type block = a and type data = data)
    : (a, data) referee
  =
  (module Referee (D))
;;

module Honest (V : View with type data = data) = struct
  include V

  type state = block

  let preferred state = state

  let init ~roots =
    match roots with
    | [ genesis ] -> genesis
    | _ -> failwith "invalid roots"
  ;;

  let puzzle_payload state =
    { sign = false
    ; parents = [ state ]
    ; data = { height = (data state).height + 1; miner = Some my_id }
    }
  ;;

  let update_head ~old candidate =
    let o = data old
    and c = data candidate in
    if c.height > o.height then candidate else old
  ;;

  let handler state = function
    | Append _ -> failwith "not implemented"
    | Network vertex -> update_head ~old:state vertex |> return
    | ProofOfWork vertex -> return ~share:[ vertex ] vertex
  ;;
end

let honest (type a) ((module V) : (a, data) view) : (a, data) node =
  Node (module Honest (V))
;;
