module type Vertex = sig
  type t

  val parents : t -> t list
  val children : t -> t list

  (** Partial ordering as imposed by {!parents} and {!children}.
      Siblings are equal.
      Parents are smaller than their children. *)
  val partial_compare : t -> t -> int

  (** Complete ordering. Compatible with {!partial_compare}.
      Parents are smaller than their children. *)
  val compare : t -> t -> int
  (** In practice this might compare something like [(depth, serial)], but this
      is not ensured. *)

  (** Physical equality. Is it the same vertex? NOT: does it store the same data? *)
  val eq : t -> t -> bool
  (** [eq a b] equals [compare a b = 0] *)

  (** Physical inequality. Is it the same vertex? NOT: does it store the same data? *)
  val neq : t -> t -> bool
  (** [neq a b] equals [compare a b = 0] *)
end

module Make (V : Vertex) : sig
  open V

  (** Recursively expand DAG in direction of {!V.children} ordered by {!V.compare}.
      The starting vertices may be included in the resulting sequence or not.
      Returned vertices are unique. *)
  val iterate_descendants : include_start:bool -> t list -> t Seq.t

  (** Recursively expand DAG in direction of {!V.parents} ordered by {!V.compare}.
      The starting vertices may be included in the resulting sequence or not.
      Returned vertices are unique. *)
  val iterate_ancestors : include_start:bool -> t list -> t Seq.t

  val common_ancestor : t -> t -> t option
  val common_ancestor' : t Seq.t -> t option
  val have_common_ancestor : t -> t -> bool

  (** Print sequence of vertices in graphviz dot format. *)
  val dot
    :  Format.formatter
    -> ?legend:(string * string) list
    -> node_attr:(t -> (string * string) list)
    -> t list
    -> unit

  (** Export sequence of vertices to iGraph/GraphML representation. *)
  val graphml : (t -> GraphML.Data.t) -> t list -> GraphML.graph

  module Exn : sig
    type exn +=
      | Malformed_DAG of
          { msg : string
          ; dot : string lazy_t
          }

    (** write [e.dot] to file as soon as error is raised *)
    val set_to_file : string -> unit

    (** convert Graphviz dot to ASCII using graph-easy if available. *)
    val dot_to_ascii : string -> string

    val raise : (t -> (string * string) list) -> t list -> string -> 'b
  end
end = struct
  open V

  let iterate order ~include_start entry_vertices =
    let expand, compare =
      match order with
      | `Downward -> children, compare
      | `Upward -> parents, Compare.neg compare
    in
    let queue = List.fold_left (fun q n -> OrderedQueue.queue n () q) in
    let entry_vertices =
      if include_start then entry_vertices else List.concat_map expand entry_vertices
    in
    let init = queue (OrderedQueue.init compare) entry_vertices in
    Seq.unfold
      (fun (last, q) ->
        OrderedQueue.dequeue q
        |> Option.map (fun (v, (), q) ->
               let fresh =
                 match last with
                 | None -> true
                 | Some last -> neq v last
               in
               let q = if fresh then expand v |> queue q else q in
               (fresh, v), (Some v, q)))
      (None, init)
    |> Seq.filter_map (fun (keep, n) -> if keep then Some n else None)
  ;;

  let iterate_descendants = iterate `Downward
  let iterate_ancestors = iterate `Upward

  let common_ancestor a b =
    let rec h a b =
      match a, b with
      | Seq.Cons (ah, aseq), Seq.Cons (bh, bseq) ->
        let cmp = compare ah bh in
        if cmp = 0
        then Some ah
        else if cmp > 0
        then h (aseq ()) b
        else if cmp < 0
        then h a (bseq ())
        else assert false
      | _ -> None
    in
    let include_start = true in
    h
      (iterate_ancestors ~include_start [ a ] ())
      (iterate_ancestors ~include_start [ b ] ())
  ;;

  let have_common_ancestor a b = common_ancestor a b |> Option.is_some

  let common_ancestor' seq =
    let open Seq in
    match seq () with
    | Nil -> None
    | Cons (hd, tl) ->
      let f = function
        | Some a -> common_ancestor a
        | None -> fun _ -> None
      in
      Seq.fold_left f (Some hd) tl
  ;;

  let dot fmt ?(legend = []) ~node_attr bl =
    let module Map = Map.Make (V) in
    let ids, levels, edges =
      match iterate_descendants ~include_start:true bl () with
      | Seq.Nil -> Map.empty, [], []
      | Cons (x, seq) ->
        let _, _, ids, level, levels, edges =
          Seq.fold_left
            (fun (i, last, ids, lvl, lvls, edges) x ->
              let edges = List.mapi (fun i c -> x, c, i) (parents x) :: edges in
              if partial_compare last x <> 0 && lvl <> []
              then i + 1, x, Map.add x i ids, [ x ], List.rev lvl :: lvls, edges
              else i + 1, x, Map.add x i ids, x :: lvl, lvls, edges)
            (1, x, Map.singleton x 0, [ x ], [], [])
            seq
        in
        ids, level :: levels |> List.rev, List.rev edges
    in
    let attr l =
      let f (k, v) = Printf.sprintf "%s=\"%s\"" k v in
      List.map f l |> String.concat " "
    in
    let open Format in
    fprintf fmt "digraph {\n";
    fprintf fmt "  rankdir = LR;\n";
    fprintf fmt "  node [shape=box];\n";
    if legend <> []
    then
      fprintf
        fmt
        "  %s;\n"
        (List.map (fun (k, v) -> Printf.sprintf "{%s|%s}" k v) legend
        |> String.concat "|"
        |> Printf.sprintf "legend [shape=Mrecord label=\"%s\"]");
    (* print nodes *)
    List.iter
      (fun level ->
        fprintf fmt "  { rank=same\n";
        List.iter
          (fun n ->
            match Map.find_opt n ids with
            | Some id -> fprintf fmt "    n%d [%s];\n" id (node_attr n |> attr)
            | None -> failwith "dot")
          level;
        fprintf fmt "  }\n")
      levels;
    (* print edges *)
    List.iter
      (List.iter (fun (c, n, i) ->
           match Map.find_opt c ids, Map.find_opt n ids with
           | Some c, Some n ->
             fprintf fmt "  n%d -> n%d [label=\"%i\", dir=back];\n" n c i
           | _ -> ()))
      edges;
    fprintf fmt "}\n"
  ;;

  let graphml data vertices : GraphML.graph =
    let module Map = Map.Make (V) in
    let ids =
      let identify ids v =
        match Map.find_opt v ids with
        | Some _id -> ids
        | None ->
          let id = Map.cardinal ids in
          Map.add v id ids
      in
      Seq.fold_left identify Map.empty (iterate_descendants ~include_start:true vertices)
    in
    let nodes, edges =
      Map.fold
        (fun vertex id (nodes, edges) ->
          let node_id i = "v" ^ string_of_int i in
          let nodes = { GraphML.id = node_id id; data = data vertex } :: nodes
          and edges =
            List.fold_left
              (fun edges child ->
                match Map.find_opt child ids with
                | Some child_id ->
                  { GraphML.src = node_id child_id; dst = node_id id; data = [] } :: edges
                | None -> edges)
              edges
              (children vertex)
          in
          nodes, edges)
        ids
        ([], [])
    in
    GraphML.{ nodes = List.rev nodes; edges = List.rev edges; kind = Directed; data = [] }
  ;;

  module Exn = struct
    type exn +=
      | Malformed_DAG of
          { msg : string
          ; dot : string lazy_t
          }

    let to_file = ref (Bos.OS.Env.var "CPR_MALFORMED_DAG_TO_FILE")
    let set_to_file f = to_file := Some f

    let dot_to_ascii dot =
      let open Bos in
      let open Rresult in
      let cmd = Cmd.(v "graph-easy") in
      OS.Cmd.exists cmd
      >>= (function
            | true ->
              OS.Cmd.(in_string dot |> run_io cmd |> out_string)
              |> Result.map fst
              |> Result.map (fun ascii -> dot ^ ascii)
            | false ->
              Result.ok
                (dot
                ^ "HINT: Install graph-easy to automatically convert this graph to ASCII."
                ))
      |> Result.get_ok
    ;;

    let () =
      Printexc.register_printer (fun exn ->
          match exn, !to_file with
          | Malformed_DAG t, Some f ->
            let oc = open_out f in
            let msg =
              try
                Printf.fprintf oc "%s" (Lazy.force t.dot);
                Printf.sprintf "Malformed_DAG: %s (DAG written to %s)" t.msg f
              with
              | _ -> Printf.sprintf "Malformed_DAG: %s (writing DAG to %s failed)" t.msg f
            in
            close_out oc;
            Some msg
          | Malformed_DAG t, None ->
            let () = Printf.eprintf "%s\n" (Lazy.force t.dot |> dot_to_ascii) in
            Some (Printf.sprintf "Malformed_DAG: %s" t.msg)
          | _ -> None)
    ;;

    let raise info nodes msg (type a) : a =
      let node_attr x =
        let label =
          info x
          |> List.map (function
                 | "", s | s, "" -> s
                 | k, v -> k ^ ": " ^ v)
          |> String.concat "\\n"
        in
        [ "label", label ]
      in
      let pp fmt = dot fmt ~legend:[ "error", msg ] ~node_attr in
      let nodes = List.concat_map parents nodes in
      let dot = lazy (Format.asprintf "%a" pp nodes) in
      raise (Malformed_DAG { msg; dot })
    ;;
  end
end

let%test_module _ =
  (module struct
    module Vertex (M : sig
      type t

      val view : t Dag.view
    end) =
    struct
      include M

      type t = M.t Dag.vertex

      let eq = Dag.vertex_eq
      let neq = Dag.vertex_neq
      let partial_compare = Dag.partial_order
      let compare = Dag.compare_vertex
      let children = Dag.children view
      let parents = Dag.parents view
    end

    let int_dag view =
      (module Vertex (struct
        type t = int

        let view = view
      end) : Vertex
        with type t = _)
    ;;

    let string_dag view =
      (module Vertex (struct
        type t = string

        let view = view
      end) : Vertex
        with type t = _)
    ;;

    let print (type a) (module V : Vertex with type t = a) ?(indent = "") to_string b =
      let open V in
      let rec h indent bl =
        match bl with
        | [ hd ] ->
          print_string indent;
          print_string "|-- ";
          print_endline (to_string hd);
          h (indent ^ "    ") (children hd)
        | hd :: tl ->
          print_string indent;
          print_string "|-- ";
          print_endline (to_string hd);
          h (indent ^ "|   ") (children hd);
          h indent tl
        | [] -> ()
      in
      print_string indent;
      print_endline (to_string b);
      h indent (children b)
    ;;

    let%expect_test _ =
      let open Dag in
      let t = create () in
      let r = append t [] 0 in
      let append r = append t [ r ] in
      let ra = append r 1
      and rb = append r 2 in
      let _raa = append ra 3
      and rba = append rb 4
      and _rbb = append rb 5 in
      let rbaa = append rba 6 in
      let _rbaaa = append rbaa 7 in
      let global = view t in
      let local = filter (fun i -> data i mod 2 = 0) global in
      let local2 = filter (fun i -> data i < 5) local in
      let to_string x = string_of_int (data x) in
      print ~indent:"global:   " (int_dag global) to_string r;
      print ~indent:"subtree:  " (int_dag global) to_string rb;
      print ~indent:"local:    " (int_dag local) to_string r;
      print ~indent:"local2:   " (int_dag local2) to_string r;
      [%expect
        {|
    global:   0
    global:   |-- 2
    global:   |   |-- 5
    global:   |   |-- 4
    global:   |       |-- 6
    global:   |           |-- 7
    global:   |-- 1
    global:       |-- 3
    subtree:  2
    subtree:  |-- 5
    subtree:  |-- 4
    subtree:      |-- 6
    subtree:          |-- 7
    local:    0
    local:    |-- 2
    local:        |-- 4
    local:            |-- 6
    local2:   0
    local2:   |-- 2
    local2:       |-- 4 |}]
    ;;

    let%expect_test "iterate" =
      let open Dag in
      let t = create () in
      let r = append t [] 0 in
      let append r = append t [ r ] in
      let ra = append r 1
      and rb = append r 2 in
      let raa = append ra 3
      and rba = append rb 4
      and rbb = append rb 5 in
      let rbaa = append rba 6 in
      let rbaaa = append rbaa 7 in
      let (module V) = int_dag (view t) in
      let print = print (module V) (fun x -> string_of_int (data x)) in
      print r;
      let print n = data n |> string_of_int |> print_string in
      print_endline "Upward";
      let module Tools = Make (V) in
      let iterate = function
        | `Upward -> Tools.iterate_ancestors ~include_start:true
        | `Downward -> Tools.iterate_descendants ~include_start:true
      in
      Seq.iter print (iterate `Upward [ rbaaa ]);
      print_newline ();
      Seq.iter print (iterate `Upward [ rbb ]);
      print_newline ();
      Seq.iter print (iterate `Upward [ raa ]);
      print_newline ();
      Seq.iter print (iterate `Upward [ ra; rb ]);
      print_newline ();
      Seq.iter print (iterate `Upward [ ra; rbb ]);
      print_newline ();
      Seq.iter print (iterate `Upward [ raa; rbb; rbaaa; rbaaa ]);
      print_newline ();
      print_endline "Downward";
      Seq.iter print (iterate `Downward [ r ]);
      print_newline ();
      Seq.iter print (iterate `Downward [ ra ]);
      print_newline ();
      Seq.iter print (iterate `Downward [ rb ]);
      print_newline ();
      Seq.iter print (iterate `Downward [ rbaa; raa ]);
      print_newline ();
      [%expect
        {|
    0
    |-- 2
    |   |-- 5
    |   |-- 4
    |       |-- 6
    |           |-- 7
    |-- 1
        |-- 3
    Upward
    76420
    520
    310
    210
    5210
    76543210
    Downward
    01234567
    13
    24567
    367 |}]
    ;;

    let%test "common_ancestor" =
      let open Dag in
      let t = create () in
      let r = append t [] 0 in
      let append r = append t [ r ] in
      let ra = append r 1
      and rb = append r 2 in
      let raa = append ra 3
      and rba = append rb 4
      and rbb = append rb 5 in
      let rbaa = append rba 6 in
      let rbaaa = append rbaa 7 in
      let (module V) = int_dag (view t) in
      let module Tools = Make (V) in
      let check a b ca =
        match Tools.common_ancestor a b with
        | Some c -> V.eq c ca
        | None -> false
      in
      List.for_all
        (fun x -> x)
        [ check ra ra ra
        ; check ra rb r
        ; check rbb rba rb
        ; check raa rba r
        ; check rbaaa rbaa rbaa
        ]
    ;;

    let%expect_test "dot" =
      let open Dag in
      let t = create () in
      let r = append t [] 0 in
      let append r = append t [ r ] in
      let ra = append r 1
      and rb = append r 2 in
      let _raa = append ra 3
      and rba = append rb 4
      and _rbb = append rb 5 in
      let rbaa = append rba 6 in
      let _rbaaa = append rbaa 7 in
      let (module V) = int_dag (view t) in
      let module Tools = Make (V) in
      Tools.dot
        Format.std_formatter
        ~node_attr:(fun n -> [ "label", data n |> string_of_int ])
        (Dag.roots t);
      [%expect
        {|
    digraph {
      rankdir = LR;
      node [shape=box];
      { rank=same
        n0 [label="0"];
      }
      { rank=same
        n1 [label="1"];
        n2 [label="2"];
      }
      { rank=same
        n3 [label="3"];
        n4 [label="4"];
        n5 [label="5"];
      }
      { rank=same
        n6 [label="6"];
      }
      { rank=same
        n7 [label="7"];
      }
      n0 -> n1 [label="0", dir=back];
      n0 -> n2 [label="0", dir=back];
      n1 -> n3 [label="0", dir=back];
      n2 -> n4 [label="0", dir=back];
      n2 -> n5 [label="0", dir=back];
      n4 -> n6 [label="0", dir=back];
      n6 -> n7 [label="0", dir=back];
    } |}]
    ;;

    let%expect_test "graphml" =
      let open Dag in
      let t = create () in
      let r = append t [] "r" in
      let append r = append t [ r ] in
      let ra = append r "ra"
      and rb = append r "rb" in
      let _raa = append ra "raa"
      and rba = append rb "rba"
      and _rbb = append rb "rbb" in
      let rbaa = append rba "rbaa" in
      let _rbaaa = append rbaa "rbaaa" in
      let (module V) = string_dag (view t) in
      let module Tools = Make (V) in
      Tools.graphml (fun v -> [ "label", GraphML.Data.String (data v) ]) [ r ]
      |> GraphML.pp_graph Format.std_formatter;
      [%expect
        {|
    { kind = Directed; data = [];
      nodes =
      [{ id = "v0"; data = [("label", String ("r"))] };
       { id = "v1"; data = [("label", String ("ra"))] };
       { id = "v2"; data = [("label", String ("rb"))] };
       { id = "v3"; data = [("label", String ("raa"))] };
       { id = "v4"; data = [("label", String ("rba"))] };
       { id = "v5"; data = [("label", String ("rbb"))] };
       { id = "v6"; data = [("label", String ("rbaa"))] };
       { id = "v7"; data = [("label", String ("rbaaa"))] }];
      edges =
      [{ src = "v2"; dst = "v0"; data = [] };
       { src = "v1"; dst = "v0"; data = [] };
       { src = "v3"; dst = "v1"; data = [] };
       { src = "v5"; dst = "v2"; data = [] };
       { src = "v4"; dst = "v2"; data = [] };
       { src = "v6"; dst = "v4"; data = [] };
       { src = "v7"; dst = "v6"; data = [] }]
      } |}]
    ;;
  end)
;;
