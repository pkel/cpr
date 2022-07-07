open Cpr_lib

module type Parameters = Bk.Parameters

module Make (Parameters : Parameters) = struct
  open Parameters

  let key = "bkll"
  let info = "Bₖ/ll with k=" ^ string_of_int k
  let puzzles_per_block = k

  type data_ = { height : int }

  and data =
    | Vote of data_
    | Block of data_

  let height = function
    | Vote x | Block x -> x.height
  ;;

  let progress x = (height x * k) + 1 |> float_of_int

  let describe = function
    | Vote _ -> "vote"
    | Block { height } -> "block " ^ string_of_int height
  ;;

  let dag_roots = [ Block { height = 0 } ]

  module Referee (V : GlobalView with type data = data) = struct
    include V

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
        (match Dag.parents view x with
        | [ x ] -> x
        | _ -> failwith "invalid dag")
    ;;

    let height x = data x |> height

    let block_height_exn x =
      match data x with
      | Block b -> b.height
      | _ -> raise (Invalid_argument "not a block")
    ;;

    let votes_only = Dag.filter is_vote view
    let blocks_only = Dag.filter is_block view

    let dag_validity vertex =
      match pow_hash vertex, data vertex, Dag.parents view vertex with
      | Some _, Vote x, [ p ] -> is_block p && x.height = height p
      | Some _, Block b, [ pblock ] when k = 1 ->
        (match data pblock with
        | Block p -> p.height + 1 = b.height
        | _ -> false)
      | Some _, Block b, pblock :: vote0 :: votes ->
        (match data pblock with
        | Block p ->
          let ordered_votes, _, nvotes =
            List.fold_left
              (fun (ok, h, i) x ->
                let h' = pow_hash x |> Option.get in
                is_vote x && h' > h && ok, h', i + 1)
              (true, pow_hash vote0 |> Option.get, 1)
              votes
          in
          p.height + 1 = b.height && nvotes = k - 1 && ordered_votes
        | _ -> false)
      | _ -> false
    ;;

    (** better is bigger *)
    let compare_blocks =
      let open Compare in
      let cmp =
        by int block_height_exn
        $ by int (fun x -> List.length (Dag.children votes_only x))
      in
      skip_eq Dag.vertex_eq cmp
    ;;

    let winner l =
      List.map last_block l
      |> Compare.first (Compare.neg compare_blocks) 1
      |> Option.get
      |> List.hd
    ;;

    let history =
      Seq.unfold (fun this ->
          Dag.parents view this
          |> fun parents -> List.nth_opt parents 0 |> Option.map (fun next -> this, next))
    ;;

    let constant_pow c : env reward_function =
     fun ~assign x ->
      if not (is_block x)
      then failwith "reward function should only be called for linear history of blocks";
      assign c x;
      Dag.parents view x |> List.iteri (fun i p -> if i > 0 then assign c p)
   ;;

    let constant_block c : env reward_function =
     fun ~assign x ->
      if not (is_block x)
      then failwith "reward function should only be called for linear history of blocks";
      assign c x
   ;;

    let reward_functions =
      let open Collection in
      empty
      |> add ~info:"1 per confirmed block" "block" (constant_block 1.)
      |> add ~info:"1 per confirmed pow solution" "constant" (constant_pow 1.)
    ;;
  end

  let referee (type a) (module V : GlobalView with type env = a and type data = data)
      : (a, data) referee
    =
    (module Referee (V))
  ;;

  module Honest (V : LocalView with type data = data) = struct
    include V
    open Referee (V)

    type state = env Dag.vertex

    let preferred state = state

    let init ~roots =
      match roots with
      | [ genesis ] -> genesis
      | _ -> failwith "invalid roots"
    ;;

    let compare_blocks =
      let open Compare in
      let cmp =
        by int block_height_exn
        $ by int (fun x -> List.length (Dag.children votes_only x))
        $ by int (fun x -> if appended_by_me x then 1 else 0)
        $ by (neg float) delivered_at
      in
      skip_eq Dag.vertex_eq cmp
    ;;

    let update_head ~preferred ~consider =
      if compare_blocks consider preferred > 0 then consider else preferred
    ;;

    let puzzle_payload preferred =
      let pow_hash_exn x = pow_hash x |> Option.get in
      let votes = Dag.children votes_only preferred in
      if List.length votes >= k - 1
      then (
        let height = block_height_exn preferred + 1 in
        let parents =
          preferred
          :: (Compare.first
                Compare.(
                  by (tuple (neg bool) float) (fun x -> appended_by_me x, delivered_at x))
                (k - 1)
                votes
             |> Option.get
             |> List.sort Compare.(by (tuple int int) pow_hash_exn))
        in
        { parents; data = Block { height }; sign = false })
      else (
        let height = height preferred in
        { parents = [ preferred ]; data = Vote { height }; sign = false })
    ;;

    let handler preferred = function
      | PuzzleSolved v ->
        if is_block v
        then { share = [ v ]; state = v }
        else { share = [ v ]; state = preferred }
      | Deliver v ->
        (* We only prefer blocks. For received votes, reconsider parent block. *)
        let consider = last_block v in
        { share = []; state = update_head ~preferred ~consider }
    ;;
  end

  let honest (type a) ((module V) : (a, data) local_view) : (a, data) node =
    Node (module Honest (V))
  ;;
end
