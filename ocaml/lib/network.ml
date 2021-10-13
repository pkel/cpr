type link =
  { dest : int
  ; delay : unit -> float
  }

type node =
  { compute : float
  ; links : link list
  }

type dissemination =
  | Simple
  | Flooding

let dissemination_to_string = function
  | Simple -> "simple"
  | Flooding -> "flooding"
;;

let dissemination_of_string s =
  match s with
  | "simple" -> Ok Simple
  | "flooding" -> Ok Flooding
  | _ ->
    StrResult.errf "Invalid dissemination strategy '%s'. Use 'simple' or 'flooding'." s
;;

type t =
  { nodes : node array
  ; dissemination : dissemination
  }

let homogeneous ~delay n : t =
  let compute = 1. /. float_of_int n in
  { nodes =
      Array.init n (fun i ->
          { links =
              List.init (n - 1) (fun j -> { dest = (if j >= i then j + 1 else j); delay })
          ; compute
          })
  ; dissemination = Simple
  }
;;

type to_graphml =
  ?node_data:(int -> GraphML.Data.t)
  -> ?edge_data:(src:int -> dst:int -> GraphML.Data.t)
  -> ?graph_data:GraphML.Data.t
  -> unit
  -> GraphML.graph

let to_graphml
    ~map_id
    t
    ?(node_data = fun _ -> [])
    ?(edge_data = fun ~src:_ ~dst:_ -> [])
    ?(graph_data = [])
    ()
  =
  let open GraphML in
  let open Data.Write in
  let data =
    graph_data |> set string "dissemination" (dissemination_to_string t.dissemination)
  and nodes, edges =
    Array.to_list t.nodes
    |> List.mapi (fun i n ->
           let node =
             let data = node_data i |> set float "compute" n.compute in
             { data; id = map_id i }
           and edges =
             List.map
               (fun l ->
                 let data =
                   edge_data ~src:i ~dst:l.dest |> set string "delay" "constant 1."
                   (* TODO do something useful here *)
                 in
                 { src = map_id i; dst = map_id l.dest; data })
               n.links
           in
           node, edges)
    |> List.split
  in
  { kind = Directed; data; nodes; edges = List.concat edges }
;;

let of_graphml graph =
  let open GraphML in
  let open StrResult.Syntax in
  let open Data.Read in
  let* dissemination, graph_data = pop string "dissemination" graph.data in
  let* dissemination = dissemination_of_string dissemination in
  let n = List.length graph.nodes in
  let map_id = Array.make n (-1)
  and node_data = Array.make n []
  and edge_data = Array.make n (Array.make n [])
  and rev_map_id = Hashtbl.create n
  and compute = Array.make n Float.nan
  and links = Array.make n [] in
  let* _ =
    List.fold_left
      (fun i (n : node) ->
        let* i = i
        and* c, data = pop float "compute" n.data in
        if Hashtbl.mem rev_map_id n.id
        then StrResult.errf "node %i defined multiple times" n.id
        else (
          Hashtbl.add rev_map_id n.id i;
          map_id.(i) <- n.id;
          node_data.(i) <- data;
          compute.(i) <- c;
          Ok (i + 1)))
      (Ok 0)
      graph.nodes
  in
  let+ () =
    let delay_of_string = Distributions.float_of_string_memoize () in
    let edge (e : edge) =
      match Hashtbl.find_opt rev_map_id e.src, Hashtbl.find_opt rev_map_id e.dst with
      | Some src, Some dest ->
        let* delay, data = pop string "delay" e.data in
        let+ delay = delay_of_string delay in
        edge_data.(src).(dest) <- data;
        links.(src) <- { dest; delay } :: links.(src)
      | _ -> StrResult.errf "edge (%i,%i) references undefined node" e.src e.dst
    in
    List.fold_left
      (fun ok (e : edge) ->
        let* () = ok in
        match graph.kind with
        | Directed -> edge e
        | Undirected ->
          let* () = edge e in
          edge { e with dst = e.src; src = e.dst })
      (Ok ())
      graph.edges
  in
  let network =
    let nodes = Array.map2 (fun compute links -> { compute; links }) compute links in
    { dissemination; nodes }
  in
  let to_graphml ?node_data:n ?edge_data:e ?graph_data:g =
    let node_data i =
      match n with
      | None -> node_data.(i)
      | Some f -> node_data.(i) @ f i
    and edge_data ~src ~dst =
      match e with
      | None -> edge_data.(src).(dst)
      | Some f -> edge_data.(src).(dst) @ f ~src ~dst
    and graph_data =
      match g with
      | None -> graph_data
      | Some d -> graph_data @ d
    and map_id i = map_id.(i) in
    to_graphml network ~map_id ~node_data ~edge_data ~graph_data
  in
  network, to_graphml
;;

let to_graphml = to_graphml ~map_id:Fun.id

let%expect_test _ =
  let t = homogeneous ~delay:(Distributions.constant 1.) 3 in
  to_graphml t () |> GraphML.pp_graph Format.std_formatter;
  [%expect
    {|
    { kind = Directed; data = [("dissemination", String ("simple"))];
      nodes =
      [{ id = 0; data = [("compute", Float (0.333333333333))] };
       { id = 1; data = [("compute", Float (0.333333333333))] };
       { id = 2; data = [("compute", Float (0.333333333333))] }];
      edges =
      [{ src = 0; dst = 1; data = [("delay", String ("constant 1."))] };
       { src = 0; dst = 2; data = [("delay", String ("constant 1."))] };
       { src = 1; dst = 0; data = [("delay", String ("constant 1."))] };
       { src = 1; dst = 2; data = [("delay", String ("constant 1."))] };
       { src = 2; dst = 0; data = [("delay", String ("constant 1."))] };
       { src = 2; dst = 1; data = [("delay", String ("constant 1."))] }]
      } |}]
;;

let%test _ =
  let t = homogeneous ~delay:(Distributions.constant 1.) 3 in
  to_graphml t () |> of_graphml |> Result.map_error failwith |> Result.is_ok
;;
