type event =
  | Appends of int
  | Learns of int
  | Shares of int
  | Receives of int

type entry =
  | Vertex of
      { id : int
      ; parents : int list
      ; signature : int option
      ; info : Info.t
      }
  | Event of int * event

type logger = float -> entry -> unit

let dummy_logger : logger = fun _ _ -> ()

module GraphLogger : sig
  type state

  val create : Network.t -> state * logger
  val entries : state -> (float * entry) list
  val to_graphml : state -> GraphML.graph
end = struct
  type state =
    { network : Network.t
    ; mutable entries : (float * entry) list
    }

  let log t time entry = t.entries <- (time, entry) :: t.entries

  let create network : state * logger =
    let state = { entries = []; network } in
    state, log state
  ;;

  let entries state = List.rev state.entries

  let to_graphml { network; entries } =
    let open GraphML in
    let open Data.Write in
    (* TODO. cherry-pick graphml edits from igraph branch. ids are strings there *)
    let network_node_id int = Int.max_int - int
    and vertex_id id = id
    and event_id =
      let i = ref (Int.max_int - Array.length network.nodes) in
      fun _ ->
        let r = !i in
        decr i;
        r
    and conv =
      List.map (fun (key, attr) ->
          let value =
            let open GraphML.Data in
            match (attr : Info.value) with
            | Bool x -> Bool x
            | String x -> String x
            | Float x -> Float x
            | Int x -> Float (float_of_int x)
          in
          key, value)
    and sprintf x = Printf.ksprintf string x in
    (* consume network *)
    let data =
      let dissemination =
        match network.dissemination with
        | Simple -> "simple"
        | Flooding -> "flooding"
      in
      [ "dissemination", string dissemination
      ; "activation_delay", float network.activation_delay
      ]
    and nodes, edges =
      let open Network in
      Array.to_list network.nodes
      |> List.mapi (fun i n ->
             let node =
               let data =
                 [ "label", sprintf "node %i" i
                 ; "compute", float n.compute
                 ; "kind", string "participant"
                 ]
               in
               { data; id = network_node_id i }
             and edges =
               List.map
                 (fun l ->
                   let data =
                     [ "delay", string (Distributions.to_string l.delay)
                     ; "kind", string "link"
                     ]
                   in
                   { src = network_node_id i; dst = network_node_id l.dest; data })
                 n.links
             in
             node, edges)
      |> List.split
    in
    (* consume events *)
    let nodes, edges, _ =
      let nodes, first =
        let id = event_id () in
        { id; data = [ "label", string "Start"; "kind", string "event" ] } :: nodes, id
      in
      List.fold_left
        (fun (nodes, edges, prev) (time, event) ->
          match event with
          | Vertex x ->
            let id = vertex_id x.id
            and data =
              [ "label", sprintf "vertex %i" x.id; "time", float time ] @ conv x.info
            in
            let nodes = { id; data } :: nodes
            and edges =
              List.fold_left
                (fun edges dst ->
                  let dst = vertex_id dst in
                  let data = [ "kind", string "hash-link" ] in
                  let link = { src = id; dst; data } in
                  link :: edges)
                edges
                x.parents
            in
            nodes, edges, prev
          | Event (node, kind_vertex) ->
            let kind, vertex =
              match kind_vertex with
              | Appends vertex -> "appends", vertex
              | Learns vertex -> "learns", vertex
              | Receives vertex -> "receives", vertex
              | Shares vertex -> "shares", vertex
            in
            let id = event_id () in
            let nodes =
              { id
              ; data =
                  [ "kind", String "event"
                  ; "label", sprintf "n%i %s v%i" node kind vertex
                  ; "time", Float time
                  ]
              }
              :: nodes
            and edges =
              [ { src = prev; dst = id; data = [ "kind", string "precedes" ] }
              ; { src = network_node_id node
                ; dst = id
                ; data = [ "kind", string "actor" ]
                }
              ; { src = id; dst = vertex_id vertex; data = [ "kind", string "action" ] }
              ]
              @ edges
            in
            nodes, edges, id)
        (nodes, List.concat edges, first)
        (List.rev entries)
    in
    { kind = Directed; data; nodes; edges }
  ;;
end
