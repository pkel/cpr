type 'a t = unit

type 'a node =
  { parents : 'a node list
  ; mutable children : 'a node list
  ; data : 'a
  }

let roots data = (), List.map (fun data -> { parents = []; children = []; data }) data

let append () parents data =
  let node' = { parents; children = []; data } in
  List.iter (fun node -> node.children <- node' :: node.children) parents;
  node'
;;

let data n = n.data

type 'a view = ('a -> bool) list

let view () : 'a view = []
let filter a b = a :: b

exception Invalid_node_argument

let visible view n = List.for_all (fun flt -> flt n.data) view
let protect f view n = if visible view n then f view n else raise Invalid_node_argument
let parents view n = List.filter (visible view) n.parents
let parents n = protect parents n
let children view b = List.filter (visible view) b.children
let children v = protect children v

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
  let append t r = append t [ r ] in
  let t, rs = roots [ 0 ] in
  let r = List.hd rs in
  let ra = append t r 1
  and rb = append t r 2 in
  let _raa = append t ra 3
  and rba = append t rb 4
  and _rbb = append t rb 5 in
  let rbaa = append t rba 6 in
  let _rbaaa = append t rbaa 7 in
  let global = view t in
  let local = filter (fun i -> i mod 2 = 0) global in
  print ~indent:"global:   " global string_of_int r;
  print ~indent:"subtree:  " global string_of_int rb;
  print ~indent:"local:    " local string_of_int r;
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
    local:            |-- 6 |}]
;;
