(* D. Guo and L. Ren. Bitcoin’s latency–security analysis made simple. AFT ’22.

   (Arxiv v1) *)

open Float

let int = float_of_int

let factorial n =
  let acc = ref 1. in
  for i = 2 to n do
    acc := int i *. !acc
  done;
  !acc
;;

let choose n k = factorial n /. (factorial k *. factorial (n - k))

type params =
  { k : int (* confimation depth *)
  ; delta : float (* message delay bound *)
  ; lambda : float (* total mining rate *)
  ; rho : float (* fraction of honest mining power *)
  }

let p x = x.rho *. exp (-1. *. x.lambda *. x.delta)

let check x =
  assert (x.k > 0);
  assert (x.delta >= 0.);
  assert (x.lambda >= 0.);
  assert (x.rho >= 0.);
  assert (x.rho <= 1.);
  assert (p x > 0.5)
;;

let t1upper x =
  check x;
  let p = p x in
  let a = 2. +. (2. *. sqrt (p /. (1. -. p)))
  and b = 4. *. p *. (1. -. p) in
  a *. (b ** int x.k)
;;

let t1lower x =
  check x;
  let a = 1. /. sqrt (int x.k)
  and b = 4. *. x.rho *. (1. -. x.rho) in
  a *. (b ** int x.k)
;;

(* geometric distribution with i trials and success rate 1 - (1-p) / p = (2p-1) / p *)
let t2P1 i p =
  let q = 1. -. p in
  ((q /. p) ** int (i - 1)) *. (1. -. (q /. p))
;;

let t2F1 i p =
  let q = 1. -. p in
  (q /. p) ** int i
;;

let t2P2 j n q = choose n j *. (q ** int j) *. ((1. -. q) ** int (n - j))

let sum low high f =
  let sum = ref 0. in
  for i = low to high do
    sum := !sum +. f i
  done;
  !sum
;;

let t2F2 j n q = sum (j + 1) n (fun l -> t2P2 l n q)

let t2upper x =
  check x;
  let p = p x
  and k = x.k in
  t2F1 k p
  +. sum 1 k (fun i ->
         t2P1 i p
         *. (t2F2 (k - i) ((2 * k) + 1 - i) (1. -. p)
            +. sum 0 (k - i) (fun j ->
                   t2P2 j ((2 * k) + 1 - i) (1. -. p)
                   *. t2F1 ((2 * k) + 1 - (2 * i) - (2 * j)) p)))
;;

let t2lower x =
  check x;
  let p = x.rho
  and k = x.k in
  t2F1 k p
  +. sum 1 k (fun i ->
         t2P1 i p
         *. (t2F2 (k - i) ((2 * k) + 1 - i) (1. -. p)
            +. sum 0 (k - i) (fun j ->
                   t2P2 j ((2 * k) + 1 - i) (1. -. p)
                   *. t2F1 ((2 * k) + 2 - (2 * i) - (2 * j)) p)))
;;
