type 'a data =
  { value: 'a
  ; visibility: bool array (* could be a bitfield *)
  }

type 'a event =
  | Activation
  | Deliver of 'a data Dag.node

type ('a, 'b) node =
  { preferred: 'a data Dag.node
  ; state: 'b
  }

type ('a, 'b) state =
  { tree: 'a Dag.t
  ; queue: (float, 'a event) OrderedQueue.t
  ; nodes: ('a, 'b) node array
  }

module Interface = struct
  exception Invalid_extension

  type ('global, 'local, 'pow) context =
    { view: ('global, 'local) Dag.view
    ; release: 'global Dag.node -> unit
    ; extend_dag: ?pow:'pow -> 'global Dag.node list -> 'local -> 'global Dag.node
    }

  type ('global, 'pow) event =
    | Activate of 'pow
    | Deliver of 'global Dag.node

  type ('global, 'local, 'state, 'pow) protocol =
    { event_handler: ('global, 'local, 'pow) context -> 'state
        -> ('global, 'pow) event -> 'state
    ; dag_invariant: pow:bool -> parents:'local list -> child:'local -> bool
    }
end

module Nakamoto = struct
  open Interface
  type block = { height: int }

  let dag_invariant ~pow ~parents ~child =
    match pow, parents with
    | true, [ p ] -> child.height = p.height + 1
    | _ -> false

  let have_common_ancestor view =
    let rec h a b =
      if a == b then true
      else
        let a' = Dag.data view a and b' = Dag.data view b in
        if a'.height = b'.height then
          match Dag.parents view a, Dag.parents view b with
          | [ a ], [ b ] -> h a b
          | [], _
          | _, [] -> false
          | _ -> failwith "invalid dag"
        else if a'.height > b'.height then
          match Dag.parents view a with
          | [ a ] -> h a b
          | [] -> false
          | _ -> failwith "invalid dag"
        else
          match Dag.parents view b with
          | [ b ] -> h a b
          | [] -> false
          | _ -> failwith "invalid dag"
    in h

  let leaves view gnode =
    let rec h acc gnode =
      match Dag.children view gnode with
      | [] -> gnode :: acc
      | l -> List.fold_left h acc l
    in h [] gnode

  let event_handler ctx preferred = function
    | Activate pow ->
      let head = Dag.data ctx.view preferred in
      let head' = ctx.extend_dag ~pow [preferred] {height= head.height + 1} in
      ctx.release head';
      head'
    | Deliver gnode ->
      (* Only consider gnode if its heritage is visible. *)
      if have_common_ancestor ctx.view gnode preferred then
        let consider preferred gnode =
          let node = Dag.data ctx.view gnode
          and head = Dag.data ctx.view preferred in
          if node.height > head.height then
            gnode
          else
            preferred
        in
        (* delayed gnode might connect nodes delivered previously *)
        List.fold_left consider preferred (leaves ctx.view gnode)
      else
        preferred

  let protocol: _ protocol =
    { dag_invariant; event_handler }
end

module B_k_leaderless = struct
  open Interface

  type block = { height: int }

  type node =
    | Vote
    | Block of block

  let dag_invariant ~k ~pow ~parents ~child =
    match pow, parents, child with
    | true, [ Block _ ], Vote -> true
    | true, Block b :: votes, Block b' ->
      List.for_all (function Vote -> true | _ -> false) votes &&
      List.length (votes) = k &&
      b.height + 1 = b'.height
    | _ -> false

  let vote_children view block =
    Dag.children view block |> List.filter (fun node ->
        match Dag.data view node with
        | Vote -> true
        | Block _ -> false
      )

  let block_data_exn view node =
    match Dag.data view node with
    | Block b -> b
    | _ -> raise (Invalid_argument "not a block")

  let first n =
    let rec h n acc l =
      if n <= 0 then List.rev acc
      else match l with
        | [] -> raise (Invalid_argument "list too short")
        | hd :: tl -> h (n - 1) (hd :: acc) tl
    in h n []

  let event_handler ~k ctx preferred = function
    | Activate pow ->
      let votes = vote_children ctx.view preferred in
      if (List.length votes >= k) then
        let head = block_data_exn ctx.view preferred in
        let head' =
          ctx.extend_dag ~pow
            (preferred :: (first k votes))
            (Block { height= head.height + 1 })
        in
        ctx.release head';
        head'
      else
        let vote = ctx.extend_dag [preferred] Vote in
        ctx.release vote;
        preferred
    | Deliver gnode ->
      (* TODO: Preference can break when order of messages is off. *)
      (* TODO: Only prefer gnode if its heritage is visible. *)
      (* TODO: Consider gnode's offspring. *)
      let head = block_data_exn ctx.view preferred in
      let update_head (gblock, block) =
        if (block.height > head.height) ||
           ((block.height = head.height) &&
            (vote_children ctx.view gblock |> List.length) >
            (vote_children ctx.view preferred |> List.length))
        then gblock
        else preferred
      in
      match Dag.data ctx.view gnode with
      | Vote ->
        ( match Dag.parents ctx.view gnode with
          | [] -> preferred (* parent not visible yet *)
          | [ gnode ] ->
            let node = block_data_exn ctx.view gnode in
            update_head (gnode, node)
          | _ -> failwith "invalid dag"
        )
      | Block b -> update_head (gnode, b)

  let protocol ~k : _ protocol =
    { dag_invariant = dag_invariant ~k
    ; event_handler = event_handler ~k
    }
end
