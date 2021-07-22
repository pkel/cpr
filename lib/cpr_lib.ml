module Dag = Dag
module Distributions = Distributions
module Network = Network
module Protocol = Protocol
module Simulator = Simulator

module Compare_by : sig
  type 'a cmp = 'a -> 'a -> int

  val int : ('a -> int) -> 'a cmp
  val float : ('a -> float) -> 'a cmp

  (** [disambiguate cmp0 cmp1] disambiguates [cmp0 a b = 0] using [cmp1]. The latter is
      evaluated lazily. *)
  val disambiguate : 'a cmp -> 'a cmp -> 'a cmp

  (** infix operator for {!disambiguate} *)
  val ( $ ) : 'a cmp -> 'a cmp -> 'a cmp
end = struct
  type 'a cmp = 'a -> 'a -> int

  let int p a b = Int.compare (p a) (p b)
  let float p a b = Float.compare (p a) (p b)

  let disambiguate cmp0 cmp1 a b =
    match cmp0 a b with
    | 0 -> cmp1 a b
    | x -> x
  ;;

  let ( $ ) = disambiguate
end

let%expect_test _ =
  let open Compare_by in
  let cmp = int fst $ float snd in
  let shuffle a =
    List.map (fun e -> Random.bits (), e) a |> List.sort (int fst) |> List.map snd
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
