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

let compare_vertex a =
  let open Compare in
  (by int (function x -> x.depth) $ by int (function x -> x.serial)) a
;;

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
    local2:       |-- 4
    |}]
;;
