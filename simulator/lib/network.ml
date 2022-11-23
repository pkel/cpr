open Rresult

type link =
  { dest : int
  ; delay : float Distributions.iid
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
  | _ -> R.error_msgf "Invalid dissemination strategy '%s'. Use 'simple' or 'flooding'." s
;;

type t =
  { nodes : node array
  ; dissemination : dissemination
  ; activation_delay : float
  }

module T = struct
  let symmetric_clique ~activation_delay ~propagation_delay:delay n : t =
    let compute = 1. /. float_of_int n in
    { nodes =
        Array.init n (fun i ->
            { links =
                List.init (n - 1) (fun j ->
                    { dest = (if j >= i then j + 1 else j); delay })
            ; compute
            })
    ; dissemination = Simple
    ; activation_delay
    }
  ;;

  let two_agents ~activation_delay ~alpha =
    let delay = Distributions.constant 0. in
    { dissemination = Simple
    ; nodes =
        [| { compute = alpha; links = [ { dest = 1; delay } ] }
         ; { compute = 1. -. alpha; links = [ { dest = 0; delay } ] }
        |]
    ; activation_delay
    }
  ;;

  let selfish_mining ~alpha ~activation_delay ~gamma ~propagation_delay ~defenders =
    let defender_compute =
      if defenders < 1
      then raise (Invalid_argument "defenders must be greater zero.")
      else (1. -. alpha) /. float_of_int defenders
    in
    let attacker_msg_delay =
      if gamma > 1. -. defender_compute
      then
        raise
          (Invalid_argument "gamma must not be greater ( 1 - (1 - alpha) / defenders )")
      else (
        let gamma' = gamma +. defender_compute in
        let lower = propagation_delay *. (1. -. gamma')
        and upper = propagation_delay *. (2. -. gamma') in
        Distributions.uniform ~lower ~upper)
    in
    let n = defenders + 1 in
    let links src =
      List.filter
        (fun l -> l.dest <> src)
        (if src = 0
        then
          (* attacker messages take random time to model gamma *)
          List.init n (fun dest -> { dest; delay = attacker_msg_delay })
        else
          List.init n (fun dest ->
              if dest = 0
              then
                (* attacker receives messages immediately *)
                { dest; delay = Distributions.constant 0. }
              else
                (* defender messages take msg_delay time *)
                { dest; delay = Distributions.constant propagation_delay }))
    in
    { dissemination = Simple
    ; nodes =
        Array.init n (fun i ->
            let links = links i in
            if i = 0
            then (* attacker *)
              { compute = alpha; links }
            else (* defender *) { compute = defender_compute; links })
    ; activation_delay
    }
  ;;
end

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
    graph_data
    |> set string "dissemination" (dissemination_to_string t.dissemination)
    |> set float "activation_delay" t.activation_delay
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
                   edge_data ~src:i ~dst:l.dest
                   |> set string "delay" (Distributions.to_string l.delay)
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
  let open ResultSyntax in
  let open Data.Read in
  let* dissemination, graph_data = pop string "dissemination" graph.data in
  let* dissemination = dissemination_of_string dissemination in
  let* activation_delay, graph_data = pop float "activation_delay" graph_data in
  let n = List.length graph.nodes in
  let map_id = Array.make n None
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
        then R.error_msgf "node %s defined multiple times" n.id
        else (
          Hashtbl.add rev_map_id n.id i;
          map_id.(i) <- Some n.id;
          node_data.(i) <- data;
          compute.(i) <- c;
          Ok (i + 1)))
      (Ok 0)
      graph.nodes
  in
  let* () =
    let delay_of_string = Distributions.float_of_string_memoize () in
    let edge (e : edge) =
      match Hashtbl.find_opt rev_map_id e.src, Hashtbl.find_opt rev_map_id e.dst with
      | Some src, Some dest ->
        let* delay, data = pop string "delay" e.data in
        let+ delay = delay_of_string delay in
        edge_data.(src).(dest) <- data;
        links.(src) <- { dest; delay } :: links.(src)
      | _ -> R.error_msgf "edge (%s,%s) references undefined node" e.src e.dst
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
    { dissemination; nodes; activation_delay }
  in
  let+ map_id =
    if Array.mem None map_id
    then
      R.error_msg "Something's off with the node IDs. Maybe some node was defined twice?"
    else Ok (Array.map Option.get map_id)
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

let to_graphml = to_graphml ~map_id:(fun i -> "n" ^ string_of_int (i + 1))

let%expect_test _ =
  let t =
    T.symmetric_clique
      ~activation_delay:1.
      ~propagation_delay:(Distributions.uniform ~lower:0.6 ~upper:1.4)
      3
  in
  to_graphml t () |> GraphML.pp_graph Format.std_formatter;
  [%expect
    {|
    { kind = Directed;
      data =
      [("activation_delay", Float (1.)); ("dissemination", String ("simple"))];
      nodes =
      [{ id = "n1"; data = [("compute", Float (0.333333333333))] };
       { id = "n2"; data = [("compute", Float (0.333333333333))] };
       { id = "n3"; data = [("compute", Float (0.333333333333))] }];
      edges =
      [{ src = "n1"; dst = "n2"; data = [("delay", String ("uniform 0.6 1.4"))] };
       { src = "n1"; dst = "n3"; data = [("delay", String ("uniform 0.6 1.4"))] };
       { src = "n2"; dst = "n1"; data = [("delay", String ("uniform 0.6 1.4"))] };
       { src = "n2"; dst = "n3"; data = [("delay", String ("uniform 0.6 1.4"))] };
       { src = "n3"; dst = "n1"; data = [("delay", String ("uniform 0.6 1.4"))] };
       { src = "n3"; dst = "n2"; data = [("delay", String ("uniform 0.6 1.4"))] }]
      } |}]
;;

let%test _ =
  let t =
    T.symmetric_clique
      ~activation_delay:1.
      ~propagation_delay:(Distributions.constant 1.)
      3
  in
  let _g, _tograph = to_graphml t () |> of_graphml |> R.failwith_error_msg in
  true
;;
