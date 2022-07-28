(* We have to enumerate (n choose m) and find the optimal choice of sub blocks in
   Tailstorm. We are lazy, our problems are small, hence we go with brute force. But
   first, let's get the combinatorics done. *)

let factorial n =
  let x = ref 1 in
  let () =
    for i = 2 to n do
      x := !x * i
    done
  in
  !x
;;

let%test "factorial" = factorial 6 = 2 * 3 * 4 * 5 * 6

let n_choose_k n k = factorial n / factorial k / factorial (n - k)

let%test "n_choose_k" = n_choose_k 4 2 = 6

(* I'm lazy, it's late, got this from https://stackoverflow.com/a/127856 *)
let iter_n_choose_k n k f =
  let rec iter v s j =
    if j = k
    then f v
    else
      for i = s to n - 1 do
        iter (i :: v) (i + 1) (j + 1)
      done
  in
  iter [] 0 0
;;

let%test "iter_n_choose_k" =
  let i = ref 0 in
  let () = iter_n_choose_k 7 5 (fun _ -> incr i) in
  !i = n_choose_k 7 5
;;

let%expect_test "iter_n_choose_k" =
  let f l =
    print_char '[';
    List.sort Int.compare l
    |> List.map string_of_int
    |> String.concat "; "
    |> print_string;
    print_endline "]"
  in
  iter_n_choose_k 4 2 f;
  [%expect {|
    [0; 1]
    [0; 2]
    [0; 3]
    [1; 2]
    [1; 3]
    [2; 3] |}]
;;
