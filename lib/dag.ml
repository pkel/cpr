type 'a node =
  { serial : int
  ; parents : 'a node list
  ; mutable children : 'a node list
  ; data : 'a
  ; mutable depth : int
  }

type 'a t =
  { mutable size : int
  ; mutable roots : 'a node list
  }

let create () = { size = 0; roots = [] }
let roots t = t.roots

let append t parents data =
  let depth = List.fold_left (fun acc el -> max acc el.depth) 0 parents + 1 in
  let node' = { serial = t.size; parents; children = []; data; depth } in
  if parents = [] then t.roots <- node' :: t.roots;
  List.iter (fun node -> node.children <- node' :: node.children) parents;
  t.size <- t.size + 1;
  node'
;;

let data n = n.data

type 'a view = ('a -> bool) list

let view _ : 'a view = []
let filter a b = a :: b
let visible view n = List.for_all (fun flt -> flt n.data) view
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
      | [ a ], [ b ] -> h a b
      | [], _ | _, [] -> None
      | _ -> raise (Invalid_argument "DAG is not a tree"))
    else if a.depth > b.depth
    then (
      match parents view a with
      | [ a ] -> h a b
      | [] -> None
      | _ -> raise (Invalid_argument "DAG is not a tree"))
    else (
      match parents view b with
      | [ b ] -> h a b
      | [] -> None
      | _ -> raise (Invalid_argument "DAG is not a tree"))
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
