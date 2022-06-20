open Cpr_lib

module type Parameters = sig
  (** number of votes (= puzzle solutions) per block *)
  val k : int
end

module Make (Parameters : Parameters) = struct
  open Parameters

  let key = "bk"
  let info = Printf.sprintf "Bₖ with k=%i" k
  let puzzles_per_block = k

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

  let describe = function
    | Vote x -> "vote by " ^ string_of_int x.id
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
        | _ ->
          let info _v = [] in
          Dag.Exn.raise view info [ x ] "last block: invalid dag")
    ;;

    let height x = data x |> height

    let block_height_exn x =
      match data x with
      | Block b -> b.height
      | _ ->
        let info _v = [] in
        Dag.Exn.raise view info [ x ] "block_height_exn: not a block"
    ;;

    let votes_only = Dag.filter is_vote view
    let blocks_only = Dag.filter is_block view

    let dag_validity vertex =
      let has_pow x = pow_hash x |> Option.is_some
      and pow_hash x = pow_hash x |> Option.get in
      match data vertex, Dag.parents view vertex with
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
          && signed_by vertex = Some leader.id
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

    let constant_pow c : env reward_function =
     fun ~assign x ->
      match pow_hash x with
      | Some _ -> assign c x
      | None -> ()
   ;;

    let constant_block c : env reward_function =
     fun ~assign x -> if is_block x then assign c x
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

    let leader_hash_exn x =
      if not (is_block x) then raise (Invalid_argument "not a block");
      match Dag.parents view x with
      | _b :: v0 :: _ ->
        (match pow_hash v0 with
        | Some x -> x
        | None -> raise (Invalid_argument "invalid dag / vote"))
      | _ ->
        (* happens for genesis node *)
        max_pow_hash
    ;;

    let compare_blocks =
      let open Compare in
      let cmp =
        by int block_height_exn
        $ by int (fun x -> List.length (Dag.children votes_only x))
        $ by (tuple int int |> neg) leader_hash_exn
        $ by (neg float) delivered_at
      in
      skip_eq Dag.vertex_eq cmp
    ;;

    let update_head ~preferred ~consider =
      if compare_blocks consider preferred > 0 then consider else preferred
    ;;

    let quorum b =
      let pow_hash_exn x = pow_hash x |> Option.get in
      let my_hash, replace_hash, mine, nmine, theirs, ntheirs =
        List.fold_left
          (fun (my_hash, replace_hash, mine, nmine, theirs, ntheirs) x ->
            match data x with
            | Vote _ ->
              if appended_by_me x
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
          (max_pow_hash, max_pow_hash, [], 0, [], 0)
          (Dag.children view b)
      in
      if replace_hash <= my_hash || nmine + ntheirs < k
      then (* fast path *) None
      else if nmine >= k
      then Compare.first Compare.(by (tuple int int) pow_hash_exn) k mine
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
            Compare.first Compare.(by float delivered_at) (k - nmine) theirs |> Option.get
          in
          mine @ theirs
          |> List.sort Compare.(by (tuple int int) pow_hash_exn)
          |> Option.some))
    ;;

    let puzzle_payload preferred =
      { parents = [ preferred ]
      ; sign = false
      ; data = Vote { id = my_id; height = block_height_exn preferred }
      }
    ;;

    let propose b =
      quorum b
      |> Option.map (fun q ->
             let block =
               extend_dag
                 { parents = b :: q
                 ; data = Block { height = block_height_exn b + 1 }
                 ; sign = true
                 }
             in
             { state = block; share = [ block ] })
    ;;

    let handler preferred = function
      | PuzzleSolved v ->
        (match propose preferred with
        | Some ret -> ret
        | None -> { share = [ v ]; state = preferred })
      | Deliver x ->
        (* We only prefer blocks. For received votes, reconsider parent block. *)
        let b = last_block x in
        let consider =
          (* prefer best block of all blocks involved *)
          List.fold_left
            (fun preferred consider -> update_head ~preferred ~consider)
            preferred
        in
        (* propose if possible *)
        (match propose b with
        | Some ret -> { ret with state = consider [ b; ret.state ] }
        | None -> { share = []; state = consider [ b ] })
    ;;
  end

  let honest (type a) ((module V) : (a, data) local_view) : (a, data) node =
    Node (module Honest (V))
  ;;
end
