open Cpr_lib

type height_or_work =
  | Height (** Nakamoto / longest chain *)
  | Work (** GHOST / heaviest chain *)
[@@deriving variants]

module type Parameters = sig
  val preference : height_or_work
  val progress : height_or_work
end

module Make (Parameters : Parameters) = struct
  open Parameters

  let key = "ethereum"

  let info =
    Printf.sprintf
      "Ethereum's adaptation of GHOST with %s-preference and %s-progress"
      (Variants_of_height_or_work.to_name preference |> String.lowercase_ascii)
      (Variants_of_height_or_work.to_name progress |> String.lowercase_ascii)
  ;;

  let puzzles_per_block = 1

  type data =
    { height : int
    ; work : int
    }

  let height data = data.height

  let progress data =
    match progress with
    | Height -> float_of_int data.height
    | Work -> float_of_int data.work
  ;;

  let preference =
    match preference with
    | Height -> fun x -> x.height
    | Work -> fun x -> x.work
  ;;

  let describe { height; _ } = Printf.sprintf "block %i" height
  let dag_roots = [ { height = 0; work = 0 } ]

  module Referee (V : GlobalView with type data = data) = struct
    include V

    let dag_validity b =
      match pow_hash b, Dag.parents view b with
      | Some _, p :: uncles ->
        let pd = data p
        and bd = data b
        and ancestors, previous_uncles =
          let rec f generation (a, pu) b =
            if generation > 6
            then a, pu
            else (
              match Dag.parents view b with
              | [] -> b :: a, pu (* we hit the root *)
              | x :: tl -> f (generation + 1) (b :: a, tl @ pu) x)
          in
          f 0 ([], []) p
        in
        let check_height () = bd.height = pd.height + 1
        and check_work () = bd.work = pd.work + 1 + List.length uncles
        and check_recent u =
          let ud = data u in
          let k = bd.height - ud.height in
          1 <= k && k <= 6
        and check_unique_in_parents u =
          let n = List.filter (( $== ) u) (p :: uncles) |> List.length in
          n = 1
        and check_direct_child_of_ancestor u =
          match Dag.parents view u with
          | [] -> false
          | hd :: _ -> List.exists (( $== ) hd) ancestors
        and check_unique_in_chain u =
          List.for_all (( $!= ) u) ancestors && List.for_all (( $!= ) u) previous_uncles
        in
        check_height ()
        && check_work ()
        && List.for_all
             (fun u ->
               check_recent u
               && check_unique_in_parents u
               && check_direct_child_of_ancestor u
               && check_unique_in_chain u)
             uncles
      | _ -> false
    ;;

    let uncles b =
      match Dag.parents view b with
      | [] -> []
      | _hd :: tl -> tl
    ;;

    let winner l =
      let prop x = preference (data x) in
      List.fold_left (fun acc x -> if prop x > prop acc then x else acc) (List.hd l) l
    ;;

    let history =
      (* uncles are not part of the linear history *)
      Seq.unfold (fun this ->
          Dag.parents view this
          |> fun parents -> List.nth_opt parents 0 |> Option.map (fun next -> this, next))
    ;;

    let ethereum base_reward : env reward_function =
     fun ~assign v ->
      let uncles = uncles v in
      List.iter (assign (0.9375 *. base_reward)) uncles;
      let n_uncles = List.length uncles |> float_of_int in
      let r = 1. +. (n_uncles *. 0.03125 *. base_reward) in
      assign r v
   ;;

    let reward_functions =
      let open Collection in
      empty
      |> add ~info:"base reward 1; uncles yield 3.215% / 93.75%" "ethereum" (ethereum 1.)
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
      let preferred = state in
      let uncles =
        let non_uncle_ancestors, in_chain =
          let rec f generation (nua, ic) b =
            match Dag.parents view b with
            | [] -> (* we hit the root *) nua, ic
            | b :: _ as p ->
              let generation = generation + 1 in
              if generation > 6 then nua, ic else f generation (b :: nua, p @ ic) b
          in
          f 0 ([], [ preferred ]) preferred
        in
        List.fold_left
          (fun acc b ->
            let uncles =
              let not_in_chain b = List.for_all (( $!= ) b) in_chain
              and parent_block_in_chain b =
                match Dag.parents view b with
                | [] -> (* we hit the root *) false
                | b :: _ -> List.exists (( $== ) b) non_uncle_ancestors
              in
              Dag.children view b
              |> List.filter (fun candidate ->
                     not_in_chain candidate && parent_block_in_chain candidate)
            in
            uncles @ acc)
          []
          non_uncle_ancestors
      in
      let n_uncles = List.length uncles in
      let data =
        let { height; work } = data state in
        { height = height + 1; work = work + 1 + n_uncles }
      in
      { sign = false; parents = preferred :: uncles; data }
    ;;

    let handler state = function
      | PuzzleSolved vertex -> { state = vertex; share = [ vertex ] }
      | Deliver vertex ->
        let consider = data vertex
        and preferred = data state in
        { share = []
        ; state = (if preference consider > preference preferred then vertex else state)
        }
    ;;
  end

  let honest (type a) ((module V) : (a, data) local_view) : (a, data) node =
    Node (module Honest (V))
  ;;
end

module Whitepaper = Make (struct
  let preference = Height
  let progress = Height
end)

module Byzantium = Make (struct
  let preference = Height
  let progress = Work
end)
