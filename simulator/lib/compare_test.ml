open Compare

let%test "int" = int 0 2 = -1
let%test "int" = int 2 2 = 0
let%test "int" = int 2 0 = 1

let%expect_test "Compare" =
  let cmp = tuple int float in
  let shuffle a =
    List.map (fun e -> Random.bits (), e) a |> List.sort (by int fst) |> List.map snd
  in
  shuffle [ 1, 1.; 2, 1.; 0, 1.; 2, 0.; 2, 2.; 1, 0.; 0, 2.; 0, 0.; 1, 2. ]
  |> List.sort cmp
  |> List.iter (fun (i, f) -> Printf.printf "(%d,%g)\n" i f);
  [%expect
    {|
      (0,0)
      (0,1)
      (0,2)
      (1,0)
      (1,1)
      (1,2)
      (2,0)
      (2,1)
      (2,2) |}]
;;

let%test "is_sorted" = is_sorted Compare.int [ 0; 1; 2; 3 ]
let%test "is_sorted" = is_sorted Compare.int [ 0; 2; 1; 3 ] |> not
let%test "is_sorted" = is_sorted ~unique:true Compare.int [ 0; 1; 2; 3 ]
let%test "is_sorted" = is_sorted ~unique:true Compare.int [ 0; 1; 2; 2; 3 ] |> not
let%test "first" = first Compare.int 2 [ 1 ] = None
let%test "first" = first Compare.int 2 [ 19; 2; 5; 3; 41 ] = Some [ 2; 3 ]

let%test "first" =
  first Compare.int ~skip_to:(fun x -> x > 10) 2 [ 19; 2; 5; 3; 42 ] = Some [ 19; 42 ]
;;

let%test "first" = first Compare.int ~skip_to:(fun x -> x > 10) 2 [ 19; 2; 5; 3 ] = None
let%test "at_most_first" = at_most_first Compare.int 2 [ 1; 7; 3; 5; 0; 2 ] = [ 0; 1 ]
let%test "at_most_first" = at_most_first Compare.int 3 [ 7; 1 ] = [ 1; 7 ]
