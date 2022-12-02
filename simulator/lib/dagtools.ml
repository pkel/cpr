module type Vertex = sig
  type t

  val parents : t -> t list
  val children : t -> t list

  (** Complete ordering of blocks. Compatible with partial order imposed by
      {!parents} and {!children}. Parents are smaller. *)
  val compare : t -> t -> int
  (** In practice this might compare something like [(depth, serial)], but this
      is not ensured. *)

  (** Physical equality. Is it the same vertex? NOT: does it store the same data? *)
  val eq : t -> t -> bool
  (** [eq a b] equals [compare a b = 0] *)

  (** Physical inequality. Is it the same vertex? NOT: does it store the same data? *)
  val neq : t -> t -> bool
  (** [neq a b] equals [compare a b = 0] *)
end

module Make (V : Vertex) : sig
  open V

  (** Recursively expand DAG in direction of {!D.children} ordered by {!D.compare_key}.
      The starting vertices may be included in the resulting sequence or not.
      Returned vertices are unique. *)
  val iterate_descendants : include_start:bool -> t list -> t Seq.t

  (** Recursively expand DAG in direction of {!D.parents} ordered by {!D.compare_key}.
      The starting vertices may be included in the resulting sequence or not.
      Returned vertices are unique. *)
  val iterate_ancestors : include_start:bool -> t list -> t Seq.t

  val common_ancestor : t -> t -> t option
  val common_ancestor' : t Seq.t -> t option
  val have_common_ancestor : t -> t -> bool
end = struct
  open V

  let iterate order ~include_start entry_vertices =
    let expand, compare =
      match order with
      | `Downward -> children, compare
      | `Upward -> parents, Compare.neg compare
    in
    let queue = List.fold_left (fun q n -> OrderedQueue.queue n () q) in
    let entry_vertices =
      if include_start then entry_vertices else List.concat_map expand entry_vertices
    in
    let init = queue (OrderedQueue.init compare) entry_vertices in
    Seq.unfold
      (fun (last, q) ->
        OrderedQueue.dequeue q
        |> Option.map (fun (v, (), q) ->
               let fresh =
                 match last with
                 | None -> true
                 | Some last -> neq v last
               in
               let q = if fresh then expand v |> queue q else q in
               (fresh, v), (Some v, q)))
      (None, init)
    |> Seq.filter_map (fun (keep, n) -> if keep then Some n else None)
  ;;

  let iterate_descendants = iterate `Downward
  let iterate_ancestors = iterate `Upward

  let common_ancestor a b =
    let rec h a b =
      match a, b with
      | Seq.Cons (ah, aseq), Seq.Cons (bh, bseq) ->
        let cmp = compare ah bh in
        if cmp = 0
        then Some ah
        else if cmp > 0
        then h (aseq ()) b
        else if cmp < 0
        then h a (bseq ())
        else assert false
      | _ -> None
    in
    let include_start = true in
    h
      (iterate_ancestors ~include_start [ a ] ())
      (iterate_ancestors ~include_start [ b ] ())
  ;;

  let have_common_ancestor a b = common_ancestor a b |> Option.is_some

  let common_ancestor' seq =
    let open Seq in
    match seq () with
    | Nil -> None
    | Cons (hd, tl) ->
      let f = function
        | Some a -> common_ancestor a
        | None -> fun _ -> None
      in
      Seq.fold_left f (Some hd) tl
  ;;
end

let%test_module _ =
  (module struct
    module Vertex (M : sig
      val view : int Dag.view
    end) =
    struct
      include M

      type t = int Dag.vertex

      let eq = Dag.vertex_eq
      let neq = Dag.vertex_neq
      let compare = Dag.compare_vertex
      let children = Dag.children view
      let parents = Dag.parents view
    end

    let print (type a) (module V : Vertex with type t = a) ?(indent = "") to_string b =
      let open V in
      let rec h indent bl =
        match bl with
        | [ hd ] ->
          print_string indent;
          print_string "|-- ";
          print_endline (to_string hd);
          h (indent ^ "    ") (children hd)
        | hd :: tl ->
          print_string indent;
          print_string "|-- ";
          print_endline (to_string hd);
          h (indent ^ "|   ") (children hd);
          h indent tl
        | [] -> ()
      in
      print_string indent;
      print_endline (to_string b);
      h indent (children b)
    ;;

    let%expect_test _ =
      let open Dag in
      let t = create () in
      let r = append t [] 0 in
      let append r = append t [ r ] in
      let ra = append r 1
      and rb = append r 2 in
      let _raa = append ra 3
      and rba = append rb 4
      and _rbb = append rb 5 in
      let rbaa = append rba 6 in
      let _rbaaa = append rbaa 7 in
      let global = view t in
      let local = filter (fun i -> data i mod 2 = 0) global in
      let local2 = filter (fun i -> data i < 5) local in
      let module G =
        Vertex (struct
          let view = global
        end)
      in
      let module L =
        Vertex (struct
          let view = local
        end)
      in
      let module L2 =
        Vertex (struct
          let view = local2
        end)
      in
      let to_string x = string_of_int (data x) in
      print ~indent:"global:   " (module G) to_string r;
      print ~indent:"subtree:  " (module G) to_string rb;
      print ~indent:"local:    " (module L) to_string r;
      print ~indent:"local2:   " (module L2) to_string r;
      [%expect
        {|
    global:   0
    global:   |-- 2
    global:   |   |-- 5
    global:   |   |-- 4
    global:   |       |-- 6
    global:   |           |-- 7
    global:   |-- 1
    global:       |-- 3
    subtree:  2
    subtree:  |-- 5
    subtree:  |-- 4
    subtree:      |-- 6
    subtree:          |-- 7
    local:    0
    local:    |-- 2
    local:        |-- 4
    local:            |-- 6
    local2:   0
    local2:   |-- 2
    local2:       |-- 4 |}]
    ;;

    let%expect_test "iterate" =
      let open Dag in
      let t = create () in
      let r = append t [] 0 in
      let append r = append t [ r ] in
      let ra = append r 1
      and rb = append r 2 in
      let raa = append ra 3
      and rba = append rb 4
      and rbb = append rb 5 in
      let rbaa = append rba 6 in
      let rbaaa = append rbaa 7 in
      let module D =
        Vertex (struct
          let view = view t
        end)
      in
      let print = print (module D) (fun x -> string_of_int (data x)) in
      print r;
      let print n = data n |> string_of_int |> print_string in
      print_endline "Upward";
      let module Tools = Make (D) in
      let iterate = function
        | `Upward -> Tools.iterate_ancestors ~include_start:true
        | `Downward -> Tools.iterate_descendants ~include_start:true
      in
      Seq.iter print (iterate `Upward [ rbaaa ]);
      print_newline ();
      Seq.iter print (iterate `Upward [ rbb ]);
      print_newline ();
      Seq.iter print (iterate `Upward [ raa ]);
      print_newline ();
      Seq.iter print (iterate `Upward [ ra; rb ]);
      print_newline ();
      Seq.iter print (iterate `Upward [ ra; rbb ]);
      print_newline ();
      Seq.iter print (iterate `Upward [ raa; rbb; rbaaa; rbaaa ]);
      print_newline ();
      print_endline "Downward";
      Seq.iter print (iterate `Downward [ r ]);
      print_newline ();
      Seq.iter print (iterate `Downward [ ra ]);
      print_newline ();
      Seq.iter print (iterate `Downward [ rb ]);
      print_newline ();
      Seq.iter print (iterate `Downward [ rbaa; raa ]);
      print_newline ();
      [%expect
        {|
    0
    |-- 2
    |   |-- 5
    |   |-- 4
    |       |-- 6
    |           |-- 7
    |-- 1
        |-- 3
    Upward
    76420
    520
    310
    210
    5210
    76543210
    Downward
    01234567
    13
    24567
    367 |}]
    ;;
  end)
;;
