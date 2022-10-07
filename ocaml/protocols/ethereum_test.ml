(* Ethereum's uncle rule is complicated. Let's test it! *)
open Cpr_lib

module TestView (P : Protocol) = struct
  type nonrec data = P.data

  type env =
    { value : data
    ; pow_hash : (int * int) option
    ; signed_by : int option
    }

  let min_pow = 0, 0
  let max_pow = 100, 100
  let dag : env Dag.t = Dag.create ()
  let view = Dag.view dag
  let data x = (Dag.data x).value
  let pow x = (Dag.data x).pow_hash
  let signature x = (Dag.data x).signed_by
end

module Ethereum = Ethereum.Make (Ethereum.Byzantium)
module V = TestView (Ethereum)
module Ref = Ethereum.Referee (V)

let env value = V.{ value; pow_hash = Some (1, 1); signed_by = None }
let root = Dag.append V.dag [] (env { height = 43; work = 47 })

let mine parent uncles =
  let value =
    let parent = V.data parent in
    env
      Ethereum.{ height = parent.height + 1; work = parent.work + List.length uncles + 1 }
  in
  let r = Dag.append V.dag (parent :: uncles) value in
  if Ref.dag_validity r then Some r else None
;;

let mine_exn p u =
  match mine p u with
  | Some b -> b
  | None -> failwith "invalid block"
;;

let mine_should_work p u =
  match mine p u with
  | Some _b -> ()
  | None -> failwith "invalid block"
;;

let mine_should_fail p u =
  match mine p u with
  | Some _b -> failwith "invalid block accept"
  | None -> ()
;;

(* main chain: *)
let a9 = root
let a8 = mine_exn a9 []
let a7 = mine_exn a8 []
let a6 = mine_exn a7 []
let a5 = mine_exn a6 []
let a4 = mine_exn a5 []
let a3 = mine_exn a4 []
let a2 = mine_exn a3 []
let a1 = mine_exn a2 []
let a0 = mine_exn a1 []

(* direct children of ancestor *)
let b8 = mine_exn a9 []
let b7 = mine_exn a8 []
let b6 = mine_exn a7 []
let b5 = mine_exn a6 []
let b4 = mine_exn a5 []
let b3 = mine_exn a4 []
let b2 = mine_exn a3 []
let b1 = mine_exn a2 []
let b0 = mine_exn a1 []

(* indirect children of ancestor *)
let c7 = mine_exn b8 []
let c6 = mine_exn b7 []
let c5 = mine_exn b6 []
let c4 = mine_exn b5 []
let c3 = mine_exn b4 []
let c2 = mine_exn b3 []
let c1 = mine_exn b2 []
let c0 = mine_exn b1 []

(* extending main chain w/o uncles *)
let%test_unit "ethereum-dag-validity" = mine_should_work a1 []
(* including ancestor should not work *)
let%test_unit "ethereum-dag-validity" = mine_should_fail a1 [ a1 ]
let%test_unit "ethereum-dag-validity" = mine_should_fail a1 [ a2 ]
let%test_unit "ethereum-dag-validity" = mine_should_fail a1 [ a9 ]
let%test_unit "ethereum-dag-validity" = mine_should_fail a1 [ a9 ]
(* direct child of ancestors should work for 1 <= k <= 6 *)
let%test_unit "ethereum-dag-validity" = mine_should_fail a1 [ b0 ]
let%test_unit "ethereum-dag-validity" = mine_should_work a1 [ b1 ]
let%test_unit "ethereum-dag-validity" = mine_should_work a1 [ b2 ]
let%test_unit "ethereum-dag-validity" = mine_should_work a1 [ b3 ]
let%test_unit "ethereum-dag-validity" = mine_should_work a1 [ b4 ]
let%test_unit "ethereum-dag-validity" = mine_should_work a1 [ b5 ]
let%test_unit "ethereum-dag-validity" = mine_should_work a1 [ b6 ]
let%test_unit "ethereum-dag-validity" = mine_should_fail a1 [ b7 ]
let%test_unit "ethereum-dag-validity" = mine_should_fail a1 [ b8 ]
(* ... also two times *)
let%test_unit "ethereum-dag-validity" = mine_should_work a1 [ b2; b3 ]
(* ... but not a single block twice *)
let%test_unit "ethereum-dag-validity" = mine_should_fail a1 [ b2; b2 ]
(* ... or more than two uncles *)
let%test_unit "ethereum-dag-validity" = mine_should_fail a1 [ b2; b3; b4 ]
(* indirect children of ancestor should never work *)
let%test_unit "ethereum-dag-validity" = mine_should_fail a1 [ c0 ]
let%test_unit "ethereum-dag-validity" = mine_should_fail a1 [ c1 ]
let%test_unit "ethereum-dag-validity" = mine_should_fail a1 [ c2 ]
let%test_unit "ethereum-dag-validity" = mine_should_fail a1 [ c3 ]
let%test_unit "ethereum-dag-validity" = mine_should_fail a1 [ c4 ]
let%test_unit "ethereum-dag-validity" = mine_should_fail a1 [ c5 ]
let%test_unit "ethereum-dag-validity" = mine_should_fail a1 [ c6 ]
let%test_unit "ethereum-dag-validity" = mine_should_fail a1 [ c7 ]

(* Indirect double inclusion should not work. *)
let a9 = root
let a8 = mine_exn a9 []
let a7 = mine_exn a8 []
let a6 = mine_exn a7 []
let a5 = mine_exn a6 []
let a4 = mine_exn a5 []
let b4 = mine_exn a5 []
let a3 = mine_exn a4 [ b4 ]

let%test_unit "ethereum-dag-validity" = mine_should_fail a3 [ b4 ]

let a2 = mine_exn a3 []

let%test_unit "ethereum-dag-validity" = mine_should_fail a2 [ b4 ]

let a1 = mine_exn a2 []

let%test_unit "ethereum-dag-validity" = mine_should_fail a1 [ b4 ]

let _ = mine_exn a1 []
