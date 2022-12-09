(* opaque identifier for vertex within dag; enables cheap comparisons *)
type vertex_id =
  { depth : int
  ; serial : int
  }

type 'a t = 'a el Pvec.t

and 'a el =
  { data : 'a
  ; parents : vertex_id list
  ; children : vertex_id list
  }

let empty () = Pvec.empty ()
let size t = Pvec.length t

let append ~parents data vec =
  let serial = size vec in
  let open Pvec in
  let depth = List.fold_left (fun acc x -> max acc x.depth) 0 parents + 1 in
  let x = { serial; depth } in
  let vec =
    List.fold_left
      (fun vec p ->
        let n = get_exn p.serial vec in
        set_exn p.serial { n with children = x :: n.children } vec)
      vec
      parents
  in
  append { data; parents; children = [] } vec, x
;;

let update x data vec =
  let el = Pvec.get_exn x.serial vec in
  Pvec.set_exn x.serial { el with data } vec
;;

let compare_vertex = compare
let partial_order a b = compare a.depth b.depth
let vertex_eq a b = a = b
let vertex_neq a b = a <> b
let parents vec v = (Pvec.get_exn v.serial vec).parents
let children vec v = (Pvec.get_exn v.serial vec).children
let data vec v = (Pvec.get_exn v.serial vec).data

let%test_module _ =
  (module struct
    let dag = empty ()
    let dag, a = append ~parents:[] "a" dag
    let dag, b = append ~parents:[ a ] "b" dag
    let dag, c = append ~parents:[ a ] "c" dag
    let dag, d = append ~parents:[ b; c ] "d" dag
    let dag0 = dag
    let dag1 = dag
    let dag0, e = append ~parents:[ d ] "e" dag0
    let dag1, f = append ~parents:[ a; d ] "f" dag1

    let%test _ = children dag d = []
    let%test _ = children dag a = [ c; b ]
    let%test _ = children dag0 d = [ e ]
    let%test _ = children dag0 a = [ c; b ]
    let%test _ = children dag1 d = [ f ]
    let%test _ = children dag1 a = [ f; c; b ]
  end)
;;
