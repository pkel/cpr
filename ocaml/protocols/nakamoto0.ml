open Cpr_lib.Next

type data = { height : int }

let key = "nakamoto"
let info = "Nakamoto consensus"
let puzzles_per_block = 1
let height data = data.height
let describe { height } = Printf.sprintf "block %i" height
let dag_roots = [ { height = 0 } ]

let dag_validity (type a) ((module V) : (a, data) global_view) vertex =
  let open V in
  match pow_hash vertex, Dag.parents view vertex with
  | Some _, [ p ] ->
    let child = data vertex
    and p = data p in
    child.height = p.height + 1
  | _ -> false
;;

let judge (type a) ((module V) : (a, data) global_view) l =
  let height x = (V.data x).height in
  List.fold_left (fun acc x -> if height x > height acc then x else acc) (List.hd l) l
;;

module Honest (V : LocalView with type data = data) = struct
  include V

  type state = env Dag.vertex

  let preferred state = state

  let init ~roots =
    match roots with
    | [ genesis ] -> genesis
    | _ -> failwith "invalid roots"
  ;;

  let puzzle_payload state =
    { sign = false; parents = [ state ]; data = { height = (data state).height + 1 } }
  ;;

  let handler state = function
    | PuzzleSolved vertex -> { state = vertex; share = [ vertex ] }
    | Deliver vertex ->
      let consider = data vertex
      and preferred = data state in
      { share = []
      ; state = (if consider.height > preferred.height then vertex else state)
      }
  ;;
end

let honest (type a) ((module V) : (a, data) local_view) : (a, data) node =
  Node (module Honest (V))
;;

let constant c : ('env, data) reward_function = fun ~view:_ ~assign n -> assign c n

let reward_functions () =
  let open Collection in
  empty |> add ~info:"1 per confirmed block" "block" (constant 1.)
;;
