type 'a cmp = 'a -> 'a -> int

let bool = Bool.compare
let int = Int.compare
let float = Float.compare
let neg cmp a b = cmp b a

let tuple cmp0 cmp1 a b =
  match cmp0 (fst a) (fst b) with
  | 0 -> cmp1 (snd a) (snd b)
  | x -> x
;;

let by cmp p a b = cmp (p a) (p b)

let disambiguate cmp0 cmp1 a b =
  match cmp0 a b with
  | 0 -> cmp1 a b
  | x -> x
;;

let ( $ ) = disambiguate
let skip_eq eq cmp a b = if eq a b then 0 else cmp a b

let is_sorted ?(unique = false) compare =
  let check a b = if unique then compare a b < 0 else compare a b <= 0 in
  let rec f = function
    | [] | [ _ ] -> true
    | h :: h2 :: t -> if check h h2 then f (h2 :: t) else false
  in
  f
;;

let minimum compare = function
  | hd :: tl ->
    List.fold_left (fun acc el -> if compare acc el < 0 then acc else el) hd tl
    |> Option.some
  | _ -> None
;;

let%test "minimum/Some" = minimum (by int snd) [ 'a', 1; 'b', 0; 'c', 2 ] = Some ('b', 0)
let%test "minimum/None" = minimum (by int snd) [] = None

let first ?(skip_to = fun _ -> true) compare n l =
  let a = Array.of_list l in
  if Array.length a < n
  then None
  else (
    let () = Array.sort compare a in
    let i = ref 0 in
    while !i < Array.length a && not (skip_to a.(!i)) do
      incr i
    done;
    if Array.length a - !i < n
    then None
    else (
      let l = ref [] in
      for j = 0 to n - 1 do
        l := a.(n - 1 + !i - j) :: !l
      done;
      Some !l))
;;

let at_most_first compare n l =
  let a = Array.of_list l in
  let () = Array.sort compare a in
  if Array.length a < n
  then Array.to_list a
  else (
    let l = ref [] in
    for j = 0 to n - 1 do
      l := a.(n - 1 - j) :: !l
    done;
    !l)
;;
