open Cpr_lib

let incentive_schemes = Bk.incentive_schemes

module type Parameters = Bk.Parameters

module Make (Parameters : Parameters) = struct
  open Parameters

  let key = Format.asprintf "spar-%i-%a" k Options.pp incentive_scheme

  let description =
    Format.asprintf
      "Simple Parallel PoW with k=%i and %a rewards"
      k
      Options.pp
      incentive_scheme
  ;;

  let info =
    let open Info in
    [ string "family" "spar"
    ; int "k" k
    ; Options.to_string incentive_scheme |> string "incentive_scheme"
    ]
  ;;

  type data_ =
    { height : int
    ; miner : int option
    }

  and data =
    | Vote of data_
    | Block of data_

  let height = function
    | Vote x | Block x -> x.height
  ;;

  let progress x =
    match x with
    | Vote _ -> (height x * k) + 1 |> float_of_int
    | Block _ -> height x * k |> float_of_int
  ;;

  let roots = [ Block { height = 0; miner = None } ]

  module Referee (D : BlockDAG with type data = data) = struct
    include D

    let info x =
      let open Info in
      match data x with
      | Vote x -> [ string "kind" "vote"; int "height" x.height ]
      | Block x -> [ string "kind" "block"; int "height" x.height ]
    ;;

    let label x =
      match data x with
      | Vote _ -> "vote"
      | Block { height; _ } -> "block " ^ string_of_int height
    ;;

    let is_vote x =
      match data x with
      | Vote _ -> true
      | _ -> false
    ;;

    let is_block x =
      match data x with
      | Block _ -> true
      | _ -> false
    ;;

    let last_block x =
      match data x with
      | Block _ -> x
      | Vote _ ->
        (match parents x with
        | [ x ] -> x
        | _ -> failwith "invalid dag")
    ;;

    let height x = data x |> height
    let progress x = data x |> progress

    let confirming_votes x =
      assert (is_block x);
      children x |> List.filter is_vote
    ;;

    let confirmed_votes x =
      assert (is_block x);
      parents x |> List.filter is_vote
    ;;

    let validity vertex =
      match data vertex, parents vertex with
      | Vote v, [ pblock ] ->
        has_pow vertex
        && is_block pblock
        && v.height = height pblock
        && Option.is_some v.miner
      | Block b, pblock :: pvotes ->
        has_pow vertex
        && is_block pblock
        && b.height = height pblock + 1
        && Option.is_some b.miner
        && List.length pvotes = k - 1
        && List.for_all (fun x -> is_vote x && Block.eq (last_block x) pblock) pvotes
      | _ -> false
    ;;

    (** better is bigger *)
    let compare_blocks =
      let open Compare in
      let cmp = by int height $ by int (fun x -> List.length (confirming_votes x)) in
      skip_eq Block.eq cmp
    ;;

    let winner l =
      List.map last_block l
      |> Compare.first (Compare.neg compare_blocks) 1
      |> Option.get
      |> List.hd
    ;;

    let precursor this = List.nth_opt (parents this) 0

    let assign c x =
      match data x with
      | Block x | Vote x ->
        (match x.miner with
        | Some x -> [ x, c ]
        | None -> [])
    ;;

    let constant x =
      match data x with
      | Block _ -> x :: confirmed_votes x |> List.concat_map (assign 1.)
      | _ -> []
    ;;

    let block x =
      match data x with
      | Block _ -> assign (float_of_int k) x
      | _ -> []
    ;;

    let reward =
      match incentive_scheme with
      | `Block -> block
      | `Constant -> constant
    ;;
  end

  let referee (type a) (module D : BlockDAG with type block = a and type data = data)
      : (a, data) referee
    =
    (module Referee (D))
  ;;

  module Honest (V : View with type data = data) = struct
    include V
    open Referee (V)

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

    let compare_blocks =
      let open Compare in
      let cmp =
        by int height
        $ by int (fun x -> List.length (confirming_votes x))
        $ by int (fun x -> if appended_by_me x then 1 else 0)
        $ by (neg float) visible_since
      in
      skip_eq Block.eq cmp
    ;;

    let update_head ~old consider =
      assert (is_block consider);
      if compare_blocks consider old > 0 then consider else old
    ;;

    let puzzle_payload' ~vote_filter preferred =
      let votes = confirming_votes preferred |> List.filter vote_filter in
      if List.length votes >= k - 1
      then (
        let height = height preferred + 1 in
        let parents =
          preferred
          :: (Compare.first
                Compare.(
                  by (tuple (neg bool) float) (fun x -> appended_by_me x, visible_since x))
                (k - 1)
                votes
             |> Option.get)
        in
        { parents; data = Block { height; miner = Some my_id }; sign = false })
      else (
        let height = height preferred in
        { parents = [ preferred ]
        ; data = Vote { height; miner = Some my_id }
        ; sign = false
        })
    ;;

    let puzzle_payload = puzzle_payload' ~vote_filter:(Fun.const true)

    let handler preferred = function
      | Append _ -> failwith "not implemented"
      | ProofOfWork vertex | Network vertex ->
        let share =
          match visibility vertex with
          | `Withheld -> [ vertex ]
          | `Received | `Released -> []
        in
        update_head ~old:preferred (last_block vertex) |> return ~share
    ;;
  end

  let honest (type a) ((module V) : (a, data) view) : (a, data) node =
    Node (module Honest (V))
  ;;
end
