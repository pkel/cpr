type link =
  { dest : int
  ; delay : unit -> float
  }

type node =
  { compute : float
  ; links : link list
  }

type dissemination =
  | Simple
  | Flooding

type t =
  { nodes : node array
  ; dissemination : dissemination
  }

let homogeneous ~delay n : t =
  let compute = 1. /. float_of_int n in
  { nodes =
      Array.init n (fun i ->
          { links =
              List.init (n - 1) (fun j -> { dest = (if j >= i then j + 1 else j); delay })
          ; compute
          })
  ; dissemination = Simple
  }
;;

let dissemination_of_string s =
  match s with
  | "simple" -> Ok Simple
  | "flooding" -> Ok Flooding
  | _ ->
    StrResult.errf "Invalid dissemination strategy '%s'. Use 'simple' or 'flooding'." s
;;

let of_graphml graph =
  let open GraphML in
  let open StrResult.Syntax in
  let open Data.Read in
  let node_ht = Hashtbl.create 41 in
  let* dissemination = get string "dissemination" graph.data >>= dissemination_of_string
  and* n =
    List.fold_left
      (fun i (n : node) ->
        let* i = i
        and* compute = get float "compute" n.data in
        if Hashtbl.mem node_ht n.id
        then StrResult.errf "node %i defined multiple times" n.id
        else (
          Hashtbl.replace node_ht n.id (i, compute);
          Ok (i + 1)))
      (Ok 0)
      graph.nodes
  in
  let id_table = Array.make n (-1) in
  let () = Hashtbl.iter (fun id (i, _) -> id_table.(i) <- id) node_ht in
  let nodes =
    Array.map
      (fun id ->
        let compute = Hashtbl.find node_ht id |> snd in
        { compute; links = [] })
      id_table
  in
  let add_edge (e : edge) =
    match Hashtbl.find_opt node_ht e.src, Hashtbl.find_opt node_ht e.dst with
    | Some (src, _), Some (dest, _) ->
      let l = { dest; delay = Distributions.constant 1. (* TODO read *) } in
      let n = nodes.(src) in
      nodes.(src) <- { n with links = l :: n.links };
      Ok ()
    | _ -> StrResult.errf "edge (%i,%i) references undefined node" e.src e.dst
  in
  let+ () =
    List.fold_left
      (fun ok (e : edge) ->
        let* () = ok in
        match graph.kind with
        | Directed -> add_edge e
        | Undirected ->
          let* () = add_edge e in
          add_edge { e with dst = e.src; src = e.dst })
      (Ok ())
      graph.edges
  in
  { dissemination; nodes }
;;

(* TODO, add second return value:
 * n : (int -> data) -> e : (int * int -> data) -> graph
 * that enables to get data back into the loaded graphml after simulation.
 *)
