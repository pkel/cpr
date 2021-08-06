module Dag = Dag
module Distributions = Distributions
module Network = Network
module Protocol = Protocol
module PrivateAttack = PrivateAttack
module Simulator = Simulator

let ( $== ) = Dag.node_eq
let ( $!= ) = Dag.node_neq

module Compare : sig
  type 'a cmp = 'a -> 'a -> int

  val int : int cmp
  val float : float cmp
  val inv : 'a cmp -> 'a cmp
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

  let int = Int.compare
  let float = Float.compare
  let inv cmp a b = cmp b a

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
end

let%expect_test _ =
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
