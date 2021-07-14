type 'a node =
  { serial : int
  ; parents : 'a node list
  ; mutable children : 'a node list
  ; data : 'a
  ; mutable depth : int
  }

let node_eq a b = a.serial = b.serial
let id a = a.serial

type 'a t =
  { mutable size : int
  ; mutable roots : 'a node list
  }

let create () = { size = 0; roots = [] }
let roots t = t.roots
let size t = t.size

let append t parents data =
  let depth = List.fold_left (fun acc el -> max acc el.depth) 0 parents + 1 in
  let node' = { serial = t.size; parents; children = []; data; depth } in
  if parents = [] then t.roots <- node' :: t.roots;
  List.iter (fun node -> node.children <- node' :: node.children) parents;
  t.size <- t.size + 1;
  node'
;;

let data n = n.data

type 'a view = ('a node -> bool) list

let view _ : 'a view = []
let filter a b = a :: b
let visible view n = List.for_all (fun flt -> flt n) view
let parents view n = List.filter (visible view) n.parents
let children view b = List.filter (visible view) b.children

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

(* TODO test [leaves] *)
let leaves view =
  let rec h acc n =
    match children view n with
    | [] -> n :: acc
    | l -> List.fold_left h acc l
  in
  h []
;;

let common_ancestor view =
  let rec h a b =
    if a == b
    then Some a
    else if a.depth = b.depth
    then (
      match parents view a, parents view b with
      | a :: _, b :: _ -> h a b
      | [], _ | _, [] -> None)
    else if a.depth > b.depth
    then (
      match parents view a with
      | a :: _ -> h a b
      | [] -> None)
    else (
      match parents view b with
      | b :: _ -> h a b
      | [] -> None)
  in
  h
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

let iterate order view entry_nodes =
  let expand, key =
    match order with
    | `Downward -> children view, fun n -> n.depth, n.serial
    | `Upward -> parents view, fun n -> -n.depth, -n.serial
  in
  let queue = List.fold_left (fun q n -> OrderedQueue.queue (key n) n q) in
  let init = -1, queue (OrderedQueue.init compare) entry_nodes in
  Seq.unfold
    (fun (last, q) ->
      OrderedQueue.dequeue q
      |> Option.map (fun (_depth, n, q) ->
             (n.serial <> last, n), (n.serial, expand n |> queue q)))
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
  Seq.iter print (iterate `Upward global [ raa; rbb; rbaaa ]);
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

let dot fmt ?(legend = []) v ~node_attr bl =
  (* TODO: use iterators *)
  let attr l =
    let f (k, v) = Printf.sprintf "%s=\"%s\"" k v in
    List.map f l |> String.concat " "
  in
  let edges = Hashtbl.create 42
  and nodes = Hashtbl.create 42
  and levels = Hashtbl.create 42 in
  let mind = ref max_int
  and maxd = ref min_int in
  let rec f n =
    if Hashtbl.mem nodes n.serial
    then ()
    else (
      let cs = children v n in
      let () =
        mind := min !mind n.depth;
        maxd := max !maxd n.depth;
        Hashtbl.add levels n.depth n;
        Hashtbl.add nodes n.serial ();
        List.iter (fun c -> Hashtbl.replace edges (c, n) ()) cs
      in
      List.iter f cs)
  in
  List.iter f bl;
  let open Printf in
  fprintf fmt "digraph {\n";
  fprintf fmt "  rankdir = LR;\n";
  fprintf fmt "  node [shape=box];\n";
  if legend <> []
  then
    Printf.fprintf
      fmt
      "  %s;\n"
      (List.map (fun (k, v) -> Printf.sprintf "{%s|%s}" k v) legend
      |> String.concat "|"
      |> Printf.sprintf "legend [shape=Mrecord label=\"%s\"]");
  for d = !mind to !maxd do
    fprintf fmt "  { rank=same\n";
    List.iter
      (fun n -> fprintf fmt "    n%d [%s];\n" n.serial (node_attr n |> attr))
      (Hashtbl.find_all levels d);
    fprintf fmt "  }\n"
  done;
  Seq.iter
    (fun (c, n) -> fprintf fmt "  n%d -> n%d [dir=back];\n" n.serial c.serial)
    (Hashtbl.to_seq_keys edges);
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
  dot stdout global ~node_attr:(fun n -> [ "label", data n |> string_of_int ]) t.roots;
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
      n0 -> n2 [dir=back];
      n6 -> n7 [dir=back];
      n2 -> n5 [dir=back];
      n4 -> n6 [dir=back];
      n1 -> n3 [dir=back];
      n0 -> n1 [dir=back];
      n2 -> n4 [dir=back];
    } |}]
;;
