(* bool array could be bit vector *)

let prufer_of_edge_seq n seq =
  (* https://doi.org/10.4236/jsea.2009.22016 *)
  (* https://github.com/networkx/networkx/blob/ead0e65bda59862e329f2e6f1da47919c6b07ca9/networkx/algorithms/tree/coding.py#L214 *)
  let degree = Array.make n 0 in
  let father = Array.make n (-1) in
  let () =
    (* init degree and father *)
    let adj = Array.make n [] in
    let () =
      (* degree and adjacency lists *)
      Seq.iter
        (fun (a, b) ->
          adj.(a) <- b :: adj.(a);
          adj.(b) <- a :: adj.(b);
          degree.(a) <- degree.(a) + 1;
          degree.(b) <- degree.(b) + 1)
        seq
    in
    (* find parents using depth-first search *)
    let root =
      (* among all roots, pick the one with the highest label *)
      let rec f i = if degree.(i) = 1 then i else f (i - 1) in
      f (n - 1)
    and seen = Array.make n false in
    let rec f parent =
      seen.(parent) <- true;
      List.iter
        (fun child ->
          if not seen.(child)
          then (
            assert (father.(child) = -1);
            father.(child) <- parent;
            f child))
        adj.(parent)
    in
    f root
  in
  let rec next i =
    assert (i < n);
    if degree.(i) = 1 then i else next (i + 1)
  in
  let x = ref (next 0) in
  let index = ref !x in
  let prufer = Array.make (n - 2) (-1) in
  for i = 0 to n - 3 do
    let y = father.(!x) in
    prufer.(i) <- y;
    degree.(y) <- degree.(y) - 1;
    if y < !index && degree.(y) = 1
    then x := y
    else (
      x := next (!index + 1);
      index := !x)
  done;
  prufer
;;

let prufer_of_edge_seq n seq = if n < 3 then [||] else prufer_of_edge_seq n seq
let prufer_of_edge_list n lst = prufer_of_edge_seq n (List.to_seq lst)

let%test _ = prufer_of_edge_list 0 [] = [||]
let%test _ = prufer_of_edge_list 1 [] = [||]
let%test _ = prufer_of_edge_list 2 [ 0, 1 ] = [||]

let%expect_test _ =
  prufer_of_edge_list 3 [ 0, 1; 0, 2 ] |> Array.iter (Printf.printf "%d; ");
  [%expect {| 0; |}]
;;

let%expect_test _ =
  prufer_of_edge_list 3 [ 0, 1; 1, 2 ] |> Array.iter (Printf.printf "%d; ");
  [%expect {| 1; |}]
;;

let%expect_test _ =
  prufer_of_edge_list 4 [ 0, 1; 1, 2; 0, 3 ] |> Array.iter (Printf.printf "%d; ");
  [%expect {| 1; 0; |}]
;;

let%test _ =
  (* Example from https://doi.org/10.4236/jsea.2009.22016 *)
  prufer_of_edge_list 8 [ 7, 3; 6, 3; 3, 1; 1, 0; 0, 4; 2, 4; 5, 2 ]
  = [| 2; 4; 0; 1; 3; 3 |]
;;
