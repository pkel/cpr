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
  type ('global, 'local, 'state) action =
    | Broadcast of 'global Dag.node
    | Append of { append_to: 'global Dag.node list
                ; extend_with: 'local
                ; handle_new: 'global Dag.node -> 'state * ('global, 'local, 'state) action list
                }

  type ('global, 'local, 'state) protocol =
    { deliver: ('global, 'local) Dag.view -> 'state -> 'global Dag.node -> 'state * ('global, 'local, 'state) action list
    ; activate: ('global, 'local) Dag.view -> 'state -> 'state * ('global, 'local, 'state) action list
    }
end

module Nakamoto = struct
  open Interface
  type block = { height: int }

  type 'a state =
    { longest_chain: 'a Dag.node }

  let deliver view state rcv_packed =
    let rcv = Dag.data view rcv_packed
    and head = Dag.data view state.longest_chain in
    if rcv.height > head.height then
      { longest_chain= rcv_packed }, []
    else
      state, []

  let activate view state =
    let head = Dag.data view state.longest_chain in
    state, [ Append { append_to = [ state.longest_chain ]
                    ; extend_with = { height= head.height + 1 }
                    ; handle_new = fun head' ->
                        { longest_chain= head'}, [Broadcast head']
                    } ]

  let protocol : _ protocol = {deliver; activate}
end

module B_k_leaderless = struct
  open Interface

  type block = { height: int }

  type dag_node =
    | Vote
    | Block of block

  type 'a state = { preferred: 'a Dag.node }

  let parent_block view block =
    let parent_blocks =
      Dag.parents view block |> List.filter_map (fun node ->
          match Dag.data view node with
          | Vote -> None
          | Block b -> Some (node, b))
    in
    (* TODO: preference breaks when order of messages is off. Can we avoid maintaining a receive buffer? *)
    match parent_blocks with
    | [] -> None (* parent is not available locally *)
    | [ b ] -> Some b
    | _ -> failwith "invalid dag"

  let child_votes view block =
    Dag.children view block |> List.filter (fun node ->
        match Dag.data view node with
        | Vote -> true
        | Block _ -> false
      )

  let deliver view state rcv_packed =
    let head =
      match Dag.data view state.preferred with
      | Block b -> b
      | _ -> failwith "invalid preference"
    in
    let update_head (b_packed, b) =
      let state =
        if (b.height > head.height) ||
           ((b.height = head.height) &&
            (child_votes view b_packed |> List.length) >
            (child_votes view state.preferred |> List.length))
        then
          { preferred= b_packed }
        else state
      in
      state, []
    in
    match Dag.data view rcv_packed with
    (* TODO: preference breaks when order of messages is off. Can we avoid maintaining a receive buffer? *)
    | Vote ->
      ( match parent_block view rcv_packed with
        | None -> state, [] (* parent not visible yet *)
        | Some candidate -> update_head candidate
      )
    | Block b -> update_head (rcv_packed, b)

  let activate k view state =
    let child_votes = child_votes view state.preferred in
    if (List.length child_votes >= k) then
      let head =
        match Dag.data view state.preferred with
        | Block b -> b
        | _ -> failwith "invalid preference"
      in
      state, [ Append { append_to = state.preferred :: child_votes
                      ; extend_with = Block { height= head.height + 1 }
                      ; handle_new = fun head' ->
                          { preferred =  head'}, [Broadcast head']
                      } ]
    else
      state, [ Append { append_to = [state.preferred]
                      ; extend_with = Vote
                      ; handle_new = fun vote ->
                          state, [Broadcast vote]
                      } ]
end

