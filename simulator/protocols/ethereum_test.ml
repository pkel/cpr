(* Ethereum's uncle rule is complicated. Let's test it! *)
open Cpr_lib

module BlockDAG (P : Protocol) = struct
  type data = P.data

  type env =
    { value : data
    ; pow_hash : int option
    ; signed_by : int option
    }

  type block = env Dag.vertex

  let min_hash = 0
  let max_hash = 100
  let compare_hash = Int.compare
  let dag : env Dag.t = Dag.create ()
  let view = Dag.view dag
  let children = Dag.children view
  let parents = Dag.parents view
  let data x = (Dag.data x).value
  let pow x = (Dag.data x).pow_hash
  let signature x = (Dag.data x).signed_by

  type hash = int

  let raise_invalid_dag _meta _blocks msg = failwith msg

  module Block = struct
    type t = block

    let children = children
    let parents = parents
    let compare = Dag.compare_vertex
    let eq = Dag.vertex_eq
    let neq = Dag.vertex_neq
  end
end

module Ethereum = Ethereum.Make (Ethereum.Byzantium)
module D = BlockDAG (Ethereum)
module Ref = Ethereum.Referee (D)

let env value = D.{ value; pow_hash = Some 1; signed_by = None }
let root = Dag.append D.dag [] (env { height = 43; work = 47; miner = None })

let mine parent uncles =
  let value =
    let parent = D.data parent in
    env
      Ethereum.
        { height = parent.height + 1
        ; work = parent.work + List.length uncles + 1
        ; miner = Some 42
        }
  in
  let r = Dag.append D.dag (parent :: uncles) value in
  if Ref.validity r then Some r else None
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
