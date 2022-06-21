type 'a vertex =
  { serial : int
  ; parents : 'a vertex list
  ; mutable children : 'a vertex list
  ; data : 'a
  ; mutable depth : int
  }

let vertex_eq a b = a.serial = b.serial
let vertex_neq a b = a.serial <> b.serial
let id a = a.serial
let partial_order a b = compare a.depth b.depth

type 'a t =
  { mutable size : int
  ; mutable roots : 'a vertex list
  }

let create () = { size = 0; roots = [] }
let roots t = t.roots
let size t = t.size

let append t parents data =
  let depth = List.fold_left (fun acc el -> max acc el.depth) 0 parents + 1 in
  let vertex' = { serial = t.size; parents; children = []; data; depth } in
  if parents = [] then t.roots <- vertex' :: t.roots;
  List.iter (fun vertex -> vertex.children <- vertex' :: vertex.children) parents;
  t.size <- t.size + 1;
  vertex'
;;

let data n = n.data

type 'a view = ('a vertex -> bool) list

let view _ : 'a view = []
let filter a b = b @ [ a ] (* it's important to apply the filter in-order *)

let visible view n = List.for_all (fun flt -> flt n) view
let parents view n = List.filter (visible view) n.parents
let children view n = List.filter (visible view) n.children

let print ?(indent = "") v data_to_string b =
  let rec h indent bl =
    match bl with
    | [ hd ] ->
      print_string indent;
      print_string "|-- ";
      print_endline (data_to_string hd.data);
      h (indent ^ "    ") (children v hd)
    | hd :: tl ->
      print_string indent;
      print_string "|-- ";
      print_endline (data_to_string hd.data);
      h (indent ^ "|   ") (children v hd);
      h indent tl
    | [] -> ()
  in
  print_string indent;
  print_endline (data_to_string b.data);
  h indent (children v b)
;;

let leaves view =
  let rec h acc n =
    match children view n with
    | [] -> n :: acc
    | l -> List.fold_left h acc l
  in
  h []
;;

let%expect_test _ =
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
  let local = filter (fun i -> i.data mod 2 = 0) global in
  let local2 = filter (fun i -> i.data < 5) local in
  print ~indent:"global:   " global string_of_int r;
  print ~indent:"subtree:  " global string_of_int rb;
  print ~indent:"local:    " local string_of_int r;
  print ~indent:"local2:   " local2 string_of_int r;
  leaves global r |> List.iter (fun v -> print_int v.serial);
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
    local2:       |-- 4
    375 |}]
;;

let iterate order view entry_vertices =
  let expand, key =
    match order with
    | `Downward -> children view, fun n -> n.depth, n.serial
    | `Upward -> parents view, fun n -> -n.depth, -n.serial
  in
  let queue = List.fold_left (fun q n -> OrderedQueue.queue (key n) n q) in
  let init = -1, queue (OrderedQueue.init compare) entry_vertices in
  Seq.unfold
    (fun (last, q) ->
      OrderedQueue.dequeue q
      |> Option.map (fun (_depth, n, q) ->
             let fresh = n.serial <> last in
             let q = if fresh then expand n |> queue q else q in
             (fresh, n), (n.serial, q)))
    init
  |> Seq.filter_map (fun (keep, n) -> if keep then Some n else None)
;;

let%expect_test "iterate" =
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
  let global = view t in
  print global string_of_int r;
  let print n = data n |> string_of_int |> print_string in
  print_endline "Upward";
  Seq.iter print (iterate `Upward global [ rbaaa ]);
  print_newline ();
  Seq.iter print (iterate `Upward global [ rbb ]);
  print_newline ();
  Seq.iter print (iterate `Upward global [ raa ]);
  print_newline ();
  Seq.iter print (iterate `Upward global [ ra; rb ]);
  print_newline ();
  Seq.iter print (iterate `Upward global [ ra; rbb ]);
  print_newline ();
  Seq.iter print (iterate `Upward global [ raa; rbb; rbaaa; rbaaa ]);
  print_newline ();
  print_endline "Downward";
  Seq.iter print (iterate `Downward global [ r ]);
  print_newline ();
  Seq.iter print (iterate `Downward global [ ra ]);
  print_newline ();
  Seq.iter print (iterate `Downward global [ rb ]);
  print_newline ();
  Seq.iter print (iterate `Downward global [ rbaa; raa ]);
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

let iterate_descendants view = iterate `Downward view
let iterate_ancestors view = iterate `Upward view

let common_ancestor view a b =
  let rec h a b =
    match a, b with
    | Seq.Cons (ah, aseq), Seq.Cons (bh, bseq) ->
      if ah.serial = bh.serial
      then Some ah
      else if ah.depth > bh.depth
      then h (aseq ()) b
      else if ah.depth < bh.depth
      then h a (bseq ())
      else (
        let () = assert (ah.depth = bh.depth) in
        if ah.serial > bh.serial
        then h (aseq ()) b
        else (
          let () = assert (ah.serial < bh.serial) in
          h a (bseq ())))
    | _ -> None
  in
  h (iterate_ancestors view [ a ] ()) (iterate_ancestors view [ b ] ())
;;

let have_common_ancestor view a b = common_ancestor view a b |> Option.is_some

let common_ancestor' view seq =
  let open Seq in
  match seq () with
  | Nil -> None
  | Cons (hd, tl) ->
    let f = function
      | Some a -> common_ancestor view a
      | None -> fun _ -> None
    in
    Seq.fold_left f (Some hd) tl
;;

let%test "common_ancestor" =
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
  let global = view t in
  let check a b ca =
    match common_ancestor global a b with
    | Some c -> c.serial = ca.serial
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

let dot fmt ?(legend = []) v ~node_attr bl =
  let attr l =
    let f (k, v) = Printf.sprintf "%s=\"%s\"" k v in
    List.map f l |> String.concat " "
  and _, level, levels, edges =
    Seq.fold_left
      (fun (d, level, levels, edges) n ->
        let edges = List.mapi (fun i c -> n, c, i) (parents v n) :: edges in
        if d <> n.depth && level <> []
        then n.depth, [ n ], List.rev level :: levels, edges
        else n.depth, n :: level, levels, edges)
      (-1, [], [], [])
      (iterate_descendants v bl)
  in
  let levels = level :: levels |> List.rev
  and edges = List.rev edges in
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
  List.iter
    (fun level ->
      fprintf fmt "  { rank=same\n";
      List.iter
        (fun n -> fprintf fmt "    n%d [%s];\n" n.serial (node_attr n |> attr))
        level;
      fprintf fmt "  }\n")
    levels;
  List.iter
    (List.iter (fun (c, n, i) ->
         fprintf fmt "  n%d -> n%d [label=\"%i\", dir=back];\n" n.serial c.serial i))
    edges;
  fprintf fmt "}\n"
;;

let%expect_test "dot" =
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
  dot
    Format.std_formatter
    global
    ~node_attr:(fun n -> [ "label", data n |> string_of_int ])
    t.roots;
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

let graphml view data vertices : GraphML.graph =
  let nodes, edges =
    Seq.fold_left
      (fun (nodes, edges) vertex ->
        let nodes = { GraphML.id = vertex.serial; data = data vertex.data } :: nodes
        and edges =
          List.fold_left
            (fun edges child ->
              { GraphML.src = child.serial; dst = vertex.serial; data = [] } :: edges)
            edges
            (children view vertex)
        in
        nodes, edges)
      ([], [])
      (iterate_descendants view vertices)
  in
  GraphML.{ nodes = List.rev nodes; edges = List.rev edges; kind = Directed; data = [] }
;;

let%expect_test "graphml" =
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
  let global = view t in
  graphml global (fun lbl -> [ "label", GraphML.Data.String lbl ]) [ r ]
  |> GraphML.pp_graph Format.std_formatter;
  [%expect
    {|
    { kind = Directed; data = [];
      nodes =
      [{ id = 0; data = [("label", String ("r"))] };
       { id = 1; data = [("label", String ("ra"))] };
       { id = 2; data = [("label", String ("rb"))] };
       { id = 3; data = [("label", String ("raa"))] };
       { id = 4; data = [("label", String ("rba"))] };
       { id = 5; data = [("label", String ("rbb"))] };
       { id = 6; data = [("label", String ("rbaa"))] };
       { id = 7; data = [("label", String ("rbaaa"))] }];
      edges =
      [{ src = 2; dst = 0; data = [] }; { src = 1; dst = 0; data = [] };
       { src = 3; dst = 1; data = [] }; { src = 5; dst = 2; data = [] };
       { src = 4; dst = 2; data = [] }; { src = 6; dst = 4; data = [] };
       { src = 7; dst = 6; data = [] }]
      } |}]
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
              ^ "HINT: Install graph-easy to automatically convert this graph to ASCII."))
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

  let raise v info nodes msg (type a) : a =
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
    let pp fmt = dot fmt ~legend:[] v ~node_attr in
    let nodes = List.concat_map (parents v) nodes in
    let dot = lazy (Format.asprintf "%a" pp nodes) in
    raise (Malformed_DAG { msg; dot })
  ;;
end
