let version = Version.t

include Intf
module Collection = Collection
module Dag = Dag
module Distributions = Distributions
module GraphML = GraphML
module Network = Network
module Simulator = Simulator
module Simulator2 = Simulator2
module ResultSyntax = ResultSyntax

let ( $== ) = Dag.vertex_eq
let ( $!= ) = Dag.vertex_neq

module Compare : sig
  type 'a cmp = 'a -> 'a -> int

  val bool : bool cmp
  val int : int cmp
  val float : float cmp
  val neg : 'a cmp -> 'a cmp
  val tuple : 'a cmp -> 'b cmp -> ('a * 'b) cmp
  val by : 'a cmp -> ('b -> 'a) -> 'b cmp

  (** [disambiguate cmp0 cmp1] disambiguates [cmp0 a b = 0] using [cmp1]. The latter is
      evaluated lazily. *)
  val disambiguate : 'a cmp -> 'a cmp -> 'a cmp

  (** infix operator for {!disambiguate} *)
  val ( $ ) : 'a cmp -> 'a cmp -> 'a cmp

  (** Avoid expensive comparison if nodes are equal *)
  val skip_eq : ('a -> 'a -> bool) -> 'a cmp -> 'a cmp
end = struct
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

  let%test "int" = int 0 2 = -1
  let%test "int" = int 2 2 = 0
  let%test "int" = int 2 0 = 1
end

let%expect_test "Compare" =
  let open Compare in
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

let is_sorted ?(unique = false) compare =
  let check a b = if unique then compare a b < 0 else compare a b <= 0 in
  let rec f = function
    | [] | [ _ ] -> true
    | h :: h2 :: t -> if check h h2 then f (h2 :: t) else false
  in
  f
;;

let%test "is_sorted" = is_sorted Compare.int [ 0; 1; 2; 3 ]
let%test "is_sorted" = is_sorted Compare.int [ 0; 2; 1; 3 ] |> not
let%test "is_sorted" = is_sorted ~unique:true Compare.int [ 0; 1; 2; 3 ]
let%test "is_sorted" = is_sorted ~unique:true Compare.int [ 0; 1; 2; 2; 3 ] |> not

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

let%test "first" = first Compare.int 2 [ 1 ] = None
let%test "first" = first Compare.int 2 [ 19; 2; 5; 3; 41 ] = Some [ 2; 3 ]

let%test "first" =
  first Compare.int ~skip_to:(fun x -> x > 10) 2 [ 19; 2; 5; 3; 42 ] = Some [ 19; 42 ]
;;

let%test "first" = first Compare.int ~skip_to:(fun x -> x > 10) 2 [ 19; 2; 5; 3 ] = None
