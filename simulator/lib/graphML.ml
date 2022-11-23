open Rresult

module Data = struct
  type value =
    | String of string
    | Float of float
    | Bool of bool
  [@@deriving show { with_path = false }]

  type t = (string * value) list [@@deriving show { with_path = false }]

  module Read = struct
    let string = function
      | String s -> Ok s
      | Bool x -> R.error_msgf "expected string, got bool %b" x
      | Float x -> R.error_msgf "expected string, got float %f" x
    ;;

    let float = function
      | Float x -> Ok x
      | Bool x -> R.error_msgf "expected float, got bool %b" x
      | String x -> R.error_msgf "expected float, got string '%s'" x
    ;;

    let bool = function
      | Bool x -> Ok x
      | Float x -> R.error_msgf "expected bool, got float %f" x
      | String x -> R.error_msgf "expected bool, got string '%s'" x
    ;;

    let int str =
      let open ResultSyntax in
      let* x = float str in
      match Float.to_int x with
      | i -> Ok i
      | exception _ -> R.error_msgf "expected integer, got float %f" x
    ;;

    let get f str data =
      match List.assoc_opt str data with
      | None -> R.error_msgf "missing attribute '%s'" str
      | Some d ->
        R.reword_error_msg
          (fun m -> R.msgf "invalid value for attribute '%s': %s" str m)
          (f d)
    ;;

    let pop f str data =
      match get f str data with
      | Ok x -> Ok (x, List.remove_assoc str data)
      | Error e -> Error e
    ;;
  end

  module Write = struct
    let string x = String x
    let float x = Float x
    let bool x = Bool x
    let int x = float_of_int x |> float

    let set f str value data =
      let data = List.remove_assoc str data in
      (str, f value) :: data
    ;;
  end
end

type kind =
  | Directed
  | Undirected
[@@deriving show { with_path = false }]

type edge =
  { src : string
  ; dst : string
  ; data : Data.t
  }
[@@deriving show { with_path = false }]

type node =
  { id : string
  ; data : Data.t
  }
[@@deriving show { with_path = false }]

type graph =
  { kind : kind
  ; data : Data.t
  ; nodes : node list
  ; edges : edge list
  }
[@@deriving show { with_path = false }]

let graph_to_xml =
  let open Xmlm in
  let el ?(a = []) tag l : _ frag =
    `El ((("", tag), List.map (fun (k, v) -> ("", k), v) a), l)
  in
  let data ht eon (key, d) =
    let s, t =
      match d with
      | Data.String s -> s, `String
      | Float f -> string_of_float f, `Double
      | Bool true -> "true", `Boolean
      | Bool false -> "false", `Boolean
    and key' =
      match eon with
      | `Edge -> "e_" ^ key
      | `Node -> "v_" ^ key
      | `Graph -> "g_" ^ key
    in
    let () =
      match Hashtbl.find_opt ht key' with
      | None -> Hashtbl.add ht key' (t, eon, key)
      | Some (t', _, _) when t' = t -> ()
      | Some _ -> failwith (Printf.sprintf "conflicting types for key %s" key)
    in
    el "data" ~a:[ "key", key' ] [ `Data s ]
  in
  let nodes_edges_data_keys g =
    let keys = Hashtbl.create 7 in
    let edges =
      List.fold_left
        (fun edges e ->
          el
            "edge"
            ~a:[ "source", e.src; "target", e.dst ]
            (List.fold_left (fun acc d -> data keys `Edge d :: acc) [] e.data)
          :: edges)
        []
        g.edges
    and nodes =
      List.fold_left
        (fun nodes n ->
          el
            "node"
            ~a:[ "id", n.id ]
            (List.fold_left (fun acc d -> data keys `Node d :: acc) [] n.data)
          :: nodes)
        []
        g.nodes
    and data = List.fold_left (fun acc d -> data keys `Graph d :: acc) [] g.data in
    let keys =
      Hashtbl.fold
        (fun key (t, eon, name) acc ->
          let for_ =
            match eon with
            | `Edge -> "edge"
            | `Node -> "node"
            | `Graph -> "graph"
          and type_ =
            match t with
            | `Boolean -> "boolean"
            | `String -> "string"
            | `Double -> "double"
          in
          el "key" ~a:[ "id", key; "for", for_; "attr.name", name; "attr.type", type_ ] []
          :: acc)
        keys
        []
    in
    nodes, edges, data, keys
  in
  fun g ->
    match nodes_edges_data_keys g with
    | exception Failure s -> R.error_msg s
    | nodes, edges, data, keys ->
      let edgedefault =
        match g.kind with
        | Directed -> "directed"
        | Undirected -> "undirected"
      in
      Ok
        (el
           "graphml"
           ~a:
             [ "xmlns", "http://graphml.graphdrawing.org/xmlns"
             ; "xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance"
             ; ( "xsi:schemaLocation"
               , "http://graphml.graphdrawing.org/xmlns \
                  http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd" )
             ]
           (keys
           @ [ el
                 "graph"
                 ~a:[ "edgedefault", edgedefault ]
                 (List.rev data @ List.rev nodes @ List.rev edges)
             ]))
;;

let graph_of_xml =
  let open Ezxmlm in
  let keys graph =
    let ht = Hashtbl.create 7 in
    List.iter
      (fun (attrs, _) ->
        let key =
          match get_attr "id" attrs with
          | s -> s
          | exception Not_found -> failwith "missing id attribute on key"
        and for_ =
          match get_attr "for" attrs with
          | "node" -> `Node
          | "edge" -> `Edge
          | "graph" -> `Graph
          | _ -> failwith "unexpected value of \"for\" attribute on key"
          | exception Not_found -> failwith "missing \"for\" attribute on key"
        and typ =
          match get_attr "attr.type" attrs with
          | "string" -> `String
          | "double" -> `Double
          | "float" -> `Float
          | "long" -> `Long
          | "boolean" -> `Boolean
          | _ -> failwith "unexpected value of \"attr.type\" attribute on key"
          | exception Not_found -> failwith "missing \"attr.type\" attribute on key"
        and name =
          match get_attr "attr.name" attrs with
          | s -> s
          | exception Not_found -> failwith "missing \"attr.name\" attribute on key"
        in
        Hashtbl.add ht (for_, key) (typ, name))
      (members_with_attr "key" graph);
    ht
  in
  let graph xml =
    let gml =
      match member "graphml" xml with
      | x -> x
      | exception Not_found -> failwith "invalid graphml file"
    in
    let graph, ed =
      match member_with_attr "graph" gml with
      | exception Not_found -> failwith "invalid graphml file"
      | attrs, childs ->
        let ed =
          match get_attr "edgedefault" attrs with
          | exception Not_found -> Undirected
          | "undirected" -> Undirected
          | "directed" -> Directed
          | _ -> failwith "unknown value for \"edgedefault\" attribute of graph"
        in
        childs, ed
    in
    graph, keys gml, ed
  and parse typ s =
    match typ with
    | `Boolean ->
      (match bool_of_string_opt s with
      | Some b -> Data.Bool b
      | None -> failwith "invalid boolean")
    | `Double | `Float | `Long ->
      (match float_of_string_opt s with
      | Some f -> Float f
      | None -> failwith "invalid double")
    | `String -> String s
  in
  let data keys eon frags =
    List.fold_left
      (fun data (attrs, childs) ->
        let key =
          try get_attr "key" attrs with
          | Not_found -> failwith "missing \"key\" attribute on data"
        in
        let typ, name =
          match Hashtbl.find_opt keys (eon, key) with
          | None -> failwith ("unknown key: " ^ key)
          | Some x -> x
        and str =
          match childs with
          | [ `Data s ] -> s
          | _ -> failwith "non-data within data tag"
        in
        (name, parse typ str) :: data)
      []
      (members_with_attr "data" frags)
    |> List.rev
  in
  let nodes keys graph =
    List.fold_left
      (fun nodes (attrs, childs) ->
        { id = get_attr "id" attrs; data = data keys `Node childs } :: nodes)
      []
      (members_with_attr "node" graph)
    |> List.rev
  and edges keys graph =
    List.fold_left
      (fun nodes (attrs, childs) ->
        { src = get_attr "source" attrs
        ; dst = get_attr "target" attrs
        ; data = data keys `Edge childs
        }
        :: nodes)
      []
      (members_with_attr "edge" graph)
    |> List.rev
  in
  fun xml ->
    try
      let graph, keys, kind = graph xml in
      let data = data keys `Graph graph in
      let nodes = nodes keys graph in
      let edges = edges keys graph in
      Ok { nodes; edges; data; kind }
    with
    | Failure s -> R.error_msg s
;;

let load_graph p =
  let open Bos.OS in
  let open ResultSyntax in
  let* xml = File.with_ic p (fun ic () -> Ezxmlm.from_channel ic |> snd) () in
  graph_of_xml xml
;;

let write_graph g p =
  let open Bos.OS in
  let open ResultSyntax in
  let* xml = graph_to_xml g in
  File.with_oc p (fun oc () -> Ok (Ezxmlm.to_channel oc None [ xml ])) () |> Result.join
;;

let pipe_graph (f : graph -> (graph, R.msg) result) : (unit, R.msg) result =
  let open ResultSyntax in
  let* _, xml = R.trap_exn Ezxmlm.from_channel stdin |> R.error_exn_trap_to_msg in
  let* graph = graph_of_xml xml in
  let* graph = f graph in
  let* xml = graph_to_xml graph in
  Ok (Ezxmlm.to_channel stdout None [ xml ])
;;
