open Cpr_lib

let incentive_schemes = [ `Constant; `Discount ]

module type Parameters = sig
  val preference : [ `LongestChain | `HeaviestChain ]
  val progress : [ `Height | `Work ]
  val max_uncles : [ `Infinity | `Int of int ]
  val incentive_scheme : [ `Constant | `Discount ]
end

module Whitepaper = struct
  let preference = `LongestChain
  let progress = `Height
  let max_uncles = `Infinity
  let incentive_scheme = `Constant
end

module Byzantium = struct
  let preference = `HeaviestChain
  let progress = `Work
  let max_uncles = `Int 2
  let incentive_scheme = `Discount
end

module Make (Parameters : Parameters) = struct
  open Parameters

  let key =
    let open Options in
    Format.asprintf
      "eth-%a-%a-%a-%a"
      pp
      preference
      pp
      progress
      pp
      max_uncles
      pp
      incentive_scheme
  ;;

  let description =
    let open Options in
    Format.asprintf
      "Ethereum with %a-preference, %a-progress, uncle cap %a, and %a-rewards"
      pp
      preference
      pp
      progress
      pp
      max_uncles
      pp
      incentive_scheme
  ;;

  let info =
    let open Options in
    [ info "preference" preference
    ; info "progress" progress
    ; info "max_uncles" max_uncles
    ; info "incentive_scheme" incentive_scheme
    ]
  ;;

  type data =
    { height : int
    ; work : int
    ; miner : int option
    }

  let height data = data.height

  let progress data =
    match progress with
    | `Height -> float_of_int data.height
    | `Work -> float_of_int data.work
  ;;

  let preference =
    match preference with
    | `HeaviestChain -> fun x -> x.height
    | `LongestChain -> fun x -> x.work
  ;;

  let describe { height; _ } = Printf.sprintf "block %i" height
  let roots = [ { height = 0; work = 0; miner = None } ]

  module Referee (D : BlockDAG with type data = data) = struct
    include D

    let info x =
      let x = data x in
      let open Info in
      [ int "height" x.height; int "work" x.work ]
    ;;

    let label x = Printf.sprintf "block %i" (data x).height
    let height x = data x |> height
    let progress x = data x |> progress

    let validity b =
      match pow b, parents b with
      | Some _, p :: uncles ->
        let pd = data p
        and bd = data b
        and ancestors, previous_uncles =
          let rec f generation (a, pu) b =
            if generation > 6
            then a, pu
            else (
              match parents b with
              | [] -> b :: a, pu (* we hit the root *)
              | x :: tl -> f (generation + 1) (b :: a, tl @ pu) x)
          in
          f 0 ([], []) p
        in
        let check_height () = bd.height = pd.height + 1
        and check_work () = bd.work = pd.work + 1 + List.length uncles
        and check_max_uncles () =
          match max_uncles with
          | `Infinity -> true
          | `Int i -> List.length uncles <= i
        and check_recent u =
          let ud = data u in
          let k = bd.height - ud.height in
          1 <= k && k <= 6
        and check_unique_in_parents u =
          let n = List.filter (Block.eq u) (p :: uncles) |> List.length in
          n = 1
        and check_direct_child_of_ancestor u =
          match parents u with
          | [] -> false
          | hd :: _ -> List.exists (Block.eq hd) ancestors
        and check_unique_in_chain u =
          List.for_all (Block.neq u) ancestors
          && List.for_all (Block.neq u) previous_uncles
        in
        check_height ()
        && check_work ()
        && Option.is_some bd.miner
        && check_max_uncles ()
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
      match parents b with
      | [] -> []
      | _hd :: tl -> tl
    ;;

    let winner l =
      let prop x = preference (data x) in
      List.fold_left (fun acc x -> if prop x > prop acc then x else acc) (List.hd l) l
    ;;

    (* uncles are not part of the linear history *)
    let precursor this = List.nth_opt (parents this) 0

    let assign c x =
      match (data x).miner with
      | Some x -> [ x, c ]
      | None -> []
    ;;

    (* described in whitepaper *)
    let constant base_reward x =
      let uncles = uncles x in
      let n_uncles = List.length uncles |> float_of_int in
      assign (1. +. (n_uncles *. 0.03125 *. base_reward)) x
      @ List.concat_map (assign (0.9375 *. base_reward)) uncles
    ;;

    (* actually used; source: https://www.doi.org/10.1109/ICDCS.2019.00131 *)
    let discount base_reward x =
      let uncles = uncles x in
      let h = height x in
      let n_uncles = List.length uncles |> float_of_int in
      assign (1. +. (n_uncles *. 0.03125 *. base_reward)) x
      @ List.concat_map
          (fun u ->
            let delta = h - height u |> float_of_int in
            assign ((8. -. delta) /. 8. *. base_reward) u)
          uncles
    ;;

    let reward =
      match incentive_scheme with
      | `Constant -> constant 1.
      | `Discount -> discount 1.
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

    let appended_by_me x =
      match visibility x with
      | `Received -> false
      | `Withheld | `Released -> true
    ;;

    let uncle_preference =
      let open Compare in
      (* better is lower | better is first after sorting *)
      (* own over foreign *)
      (* old blocks over new *)
      by (tuple bool int) (fun x -> not (appended_by_me x), preference (data x))
    ;;

    let puzzle_payload' ~uncle_filter state =
      let preferred = state in
      let uncles =
        let non_uncle_ancestors, in_chain =
          let rec f generation (nua, ic) b =
            match parents b with
            | [] -> (* we hit the root *) nua, ic
            | b :: _ as p ->
              let generation = generation + 1 in
              if generation > 6 then nua, ic else f generation (b :: nua, p @ ic) b
          in
          f 0 ([], [ preferred ]) preferred
        in
        let max_uncles =
          match max_uncles with
          | `Infinity -> Int.max_int
          | `Int i -> i
        in
        List.fold_left
          (fun acc b ->
            let uncles =
              let not_in_chain b = List.for_all (Block.neq b) in_chain
              and parent_block_in_chain b =
                match parents b with
                | [] -> (* we hit the root *) false
                | b :: _ -> List.exists (Block.eq b) non_uncle_ancestors
              in
              children b
              |> List.filter (fun candidate ->
                     not_in_chain candidate && parent_block_in_chain candidate)
            in
            uncles @ acc)
          []
          non_uncle_ancestors
        |> List.filter uncle_filter
        |> Compare.at_most_first uncle_preference max_uncles
      in
      let n_uncles = List.length uncles in
      let data =
        let { height; work; _ } = data state in
        { height = height + 1; work = work + 1 + n_uncles; miner = Some my_id }
      in
      { sign = false; parents = preferred :: uncles; data }
    ;;

    let puzzle_payload = puzzle_payload' ~uncle_filter:(fun _ -> true)

    let update_head ~old candidate =
      let o = data old
      and c = data candidate in
      if preference c > preference o then candidate else old
    ;;

    let handler state = function
      | Append _ -> failwith "not implemented"
      | ProofOfWork vertex | Network vertex ->
        let state = update_head ~old:state vertex
        and share =
          match visibility vertex with
          | `Withheld -> [ vertex ]
          | `Received | `Released -> []
        in
        return ~share state
    ;;
  end

  let honest (type a) ((module V) : (a, data) view) : (a, data) node =
    Node (module Honest (V))
  ;;
end
