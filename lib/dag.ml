type 'a t = unit

type 'a node =
  { parents: 'a node list
  ; mutable children: 'a node list
  ; data: 'a
  }

let root data =
  let b =
    { parents = []
    ; children = []
    ; data
    }
  in (), b

let append () parents data =
  let node' =
    { parents
    ; children = []
    ; data
    }
  in
  List.iter (fun node ->
      node.children <- node' :: node.children;
    ) parents;
  node'

type ('a, 'b) view = ('a -> bool) * ('a -> 'b)

let local_view flt map () : _ view = flt, map
let global_view t = local_view (fun _ -> true) (fun x -> x) t

exception Invalid_node_argument

let protect f (flt, _ as view) n =
  if flt n.data then f view n else raise Invalid_node_argument

let data (_, map) n = map n.data
let data v = protect data v

let parents (flt, _) n = List.filter (fun x -> flt x.data) n.parents
let parents n = protect parents n

let children (flt, _) b =
    List.filter (fun b -> flt b.data) b.children
let children v = protect children v

let print ?(indent="") v data_to_string b =
  let rec h indent bl =
    match bl with
    | hd :: [] ->
      print_string indent;
      print_string "|-- ";
      print_endline (data_to_string hd.data);
      h (indent ^ "    ") (children v hd);
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

let%expect_test _ =
  let append t r = append t [r] in
  let t, r = root 0 in
  let ra = append t r 1
  and rb = append t r 2 in
  let _raa = append t ra 3
  and rba = append t rb 4
  and _rbb = append t rb 5 in
  let rbaa = append t rba 6 in
  let _rbaaa = append t rbaa 7 in
  let global = global_view t
  and local = local_view (fun i -> i mod 2 = 0) (fun x -> x) t in
  print ~indent:"global:   " global string_of_int r;
  print ~indent:"subtree:  " global string_of_int rb;
  print ~indent:"local:    " local string_of_int r;
  [%expect {|
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
    local:            |-- 6 |}]
