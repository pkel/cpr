open Cpr_lib

let key = "nakamoto"
let info = "Nakamoto consensus"
let puzzles_per_block = 1

type data = { height : int }

let height data = data.height
let progress data = float_of_int data.height
let describe { height } = Printf.sprintf "block %i" height
let dag_roots = [ { height = 0 } ]

module Referee (V : GlobalView with type data = data) = struct
  include V

  let info x =
    let open Info in
    [ "height", Int x.height ]
  ;;

  let dag_validity vertex =
    match pow vertex, Dag.parents view vertex with
    | Some _, [ p ] ->
      let child = data vertex
      and p = data p in
      child.height = p.height + 1
    | _ -> false
  ;;

  let winner l =
    let height x = (data x).height in
    List.fold_left (fun acc x -> if height x > height acc then x else acc) (List.hd l) l
  ;;

  let history =
    Seq.unfold (fun this ->
        Dag.parents view this
        |> fun parents -> List.nth_opt parents 0 |> Option.map (fun next -> this, next))
  ;;

  let constant c : env reward_function = fun ~assign n -> assign c n

  let reward_functions =
    let open Collection in
    empty |> add ~info:"1 per confirmed block" "constant" (constant 1.)
  ;;
end

let referee (type a) (module V : GlobalView with type env = a and type data = data)
    : (a, data) referee
  =
  (module Referee (V))
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
    | Append _ -> failwith "not implemented"
    | Network vertex ->
      let consider = data vertex
      and preferred = data state in
      return (if consider.height > preferred.height then vertex else state)
    | ProofOfWork vertex -> return ~share:[ vertex ] vertex
  ;;
end

let honest (type a) ((module V) : (a, data) local_view) : (a, data) node =
  Node (module Honest (V))
;;
