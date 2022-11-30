open Cpr_lib

let incentive_schemes = [ `Block; `Constant ]

module type Parameters = sig
  (** number of votes (= puzzle solutions) per block *)
  val k : int

  val incentive_scheme : [ `Block | `Constant ]
end

module Make (Parameters : Parameters) = struct
  open Parameters

  let key = Format.asprintf "bk-%i-%a" k Options.pp incentive_scheme

  let description =
    Format.asprintf "Bₖ with k=%i and %a rewards" k Options.pp incentive_scheme
  ;;

  let info =
    let open Info in
    [ string "family" "bk"; int "k" k; Options.info "incentive_scheme" incentive_scheme ]
  ;;

  type block_data = { height : int }

  type vote_data =
    { height : int
    ; id : int
    }

  type data =
    | Vote of vote_data
    | Block of block_data

  let height = function
    | Vote x -> x.height
    | Block x -> x.height
  ;;

  let progress x =
    match x with
    | Vote _ -> (height x * k) + 1 |> float_of_int
    | Block _ -> height x * k |> float_of_int
  ;;

  let roots = [ Block { height = 0 } ]

  module Referee (D : BlockDAG with type data = data) = struct
    include D

    let info x =
      let open Info in
      match data x with
      | Vote x -> [ string "kind" "vote"; int "height" x.height; int "id" x.id ]
      | Block x -> [ string "kind" "block"; int "height" x.height ]
    ;;

    let label x =
      match data x with
      | Vote _x -> "vote"
      | Block { height } -> "block " ^ string_of_int height
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
        | _ ->
          let info _v = [] in
          raise_invalid_dag info [ x ] "last block hits root")
    ;;

    let height x = data x |> height
    let progress x = data x |> progress

    let block_height_exn x =
      match data x with
      | Block b -> b.height
      | _ ->
        let info _v = [] in
        raise_invalid_dag info [ x ] "block_height_exn: not a block"
    ;;

    let confirming_votes x = children x |> List.filter is_vote
    let confirmed_votes x = parents x |> List.filter is_vote

    let validity vertex =
      let has_pow x = pow x |> Option.is_some
      and pow_hash x = pow x |> Option.get in
      match data vertex, parents vertex with
      | Vote v, [ p ] -> has_pow vertex && is_block p && v.height = height p
      | Block b, pblock :: vote0 :: votes ->
        (match data pblock, data vote0 with
        | Block p, Vote leader ->
          let ordered_votes, _, nvotes =
            List.fold_left
              (fun (ok, h, i) n ->
                let h' = pow_hash n in
                is_vote n && h' > h && ok, h', i + 1)
              (true, pow_hash vote0, 1)
              votes
          in
          p.height + 1 = b.height
          && nvotes = k
          && ordered_votes
          && signature vertex = Some leader.id
        | _ -> false)
      | _ -> false
    ;;

    (** better is bigger *)
    let compare_blocks =
      let open Compare in
      let cmp =
        by int block_height_exn $ by int (fun x -> List.length (confirming_votes x))
      in
      skip_eq block_eq cmp
    ;;

    let winner l =
      List.map last_block l
      |> Compare.first (Compare.neg compare_blocks) 1
      |> Option.get
      |> List.hd
    ;;

    let precursor this = List.nth_opt (parents this) 0

    let constant x =
      if is_block x
      then
        List.filter_map
          (fun y ->
            match data y with
            | Vote v -> Some (v.id, 1.)
            | Block _ -> None)
          (parents x)
      else []
    ;;

    let block x =
      if is_block x
      then (
        match signature x with
        | Some i -> [ i, float_of_int k ]
        | None -> [])
      else []
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

    let leader_hash_exn x =
      if not (is_block x) then raise (Invalid_argument "not a block");
      match parents x with
      | _b :: v0 :: _ ->
        (match pow v0 with
        | Some x -> x
        | None -> raise (Invalid_argument "invalid dag / vote"))
      | _ ->
        (* happens for genesis node *)
        max_hash
    ;;

    let compare_blocks ~vote_filter =
      let open Compare in
      let cmp =
        by int block_height_exn
        $ by int (fun x -> List.length (confirmed_votes x |> List.filter vote_filter))
        $ by (neg compare_hash) leader_hash_exn
        $ by (neg float) visible_since (* TODO. Maybe this should be received_at? *)
      in
      skip_eq block_eq cmp
    ;;

    let update_head ?(vote_filter = Fun.const true) ~old consider =
      if compare_blocks ~vote_filter consider old > 0 then consider else old
    ;;

    let quorum ~vote_filter b =
      let pow_hash_exn x = pow x |> Option.get in
      let my_hash, replace_hash, mine, nmine, theirs, ntheirs =
        List.fold_left
          (fun (my_hash, replace_hash, mine, nmine, theirs, ntheirs) x ->
            match data x with
            | Vote v ->
              if v.id = my_id
              then
                ( min my_hash (pow_hash_exn x)
                , replace_hash
                , x :: mine
                , nmine + 1
                , theirs
                , ntheirs )
              else my_hash, replace_hash, mine, nmine, x :: theirs, ntheirs + 1
            | Block _ ->
              my_hash, min replace_hash (leader_hash_exn x), mine, nmine, theirs, ntheirs)
          (max_hash, max_hash, [], 0, [], 0)
          (confirming_votes b |> List.filter vote_filter)
      in
      if replace_hash <= my_hash || nmine + ntheirs < k
      then (* fast path *) None
      else if nmine >= k
      then Compare.first Compare.(by compare_hash pow_hash_exn) k mine
      else (
        let theirs, ntheirs =
          List.fold_left
            (fun (theirs, ntheirs) vote ->
              if pow_hash_exn vote > my_hash
              then vote :: theirs, ntheirs + 1
              else theirs, ntheirs)
            ([], 0)
            theirs
        in
        if ntheirs < k - nmine
        then (* fast path *) None
        else (
          let theirs =
            Compare.first
              Compare.(by float visible_since (* TODO maybe use received_at? *))
              (k - nmine)
              theirs
            |> Option.get
          in
          mine @ theirs |> List.sort Compare.(by compare_hash pow_hash_exn) |> Option.some))
    ;;

    let puzzle_payload preferred =
      { parents = [ preferred ]
      ; sign = false
      ; data = Vote { id = my_id; height = block_height_exn preferred }
      }
    ;;

    let propose ?(vote_filter = Fun.const true) b =
      quorum ~vote_filter b
      |> Option.map (fun q ->
             { parents = b :: q
             ; data = Block { height = block_height_exn b + 1 }
             ; sign = true
             })
    ;;

    let handler preferred = function
      | Append x | Network x | ProofOfWork x ->
        let b = last_block x in
        let append =
          match propose b with
          | Some block -> [ block ]
          | None -> []
        and share =
          match visibility x with
          | `Withheld -> [ x ]
          | _ -> []
        in
        update_head ~old:preferred b |> return ~append ~share
    ;;
  end

  let honest (type a) ((module V) : (a, data) view) : (a, data) node =
    Node (module Honest (V))
  ;;
end
