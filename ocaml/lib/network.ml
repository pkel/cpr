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

(* TODO: add this message lazy to the result type *)
let stringify kind key = function
  | `Key_not_found -> Printf.sprintf "missing attribute '%s' on %s" key kind
  | `Type_mismatch -> Printf.sprintf "attribute '%s' has wrong type on %s" key kind
;;

let of_graphml graph =
  let open GraphML in
  let open Data.Read in
  let dissemination, errors =
    let key = "dissemination" in
    string key graph.data
    |> Result.map_error (stringify "graph" key)
    |> Result.map (function
           | "simple" -> Ok Simple
           | "flooding" -> Ok Flooding
           | _ -> Error "unknown dissemination strategy")
    |> Result.join
    |> function
    | Ok x -> x, []
    | Error e -> Simple, [ e ^ "; use simple" ]
  in
  let node_ht = Hashtbl.create 41 in
  let n, errors =
    List.fold_left
      (fun (i, errors) (n : node) ->
        let compute, errors =
          let key = "compute" in
          match double key n.data with
          | Ok x -> x, errors
          | Error `Type_mismatch ->
            1., "wrong type for 'compute' attribute; use 1." :: errors
          | Error `Key_not_found -> 1., "'compute' attribute missing; use 1." :: errors
        in
        let errors =
          if Hashtbl.mem node_ht n.id then "duplicate node id" :: errors else errors
        in
        Hashtbl.replace node_ht n.id (i, compute);
        i + 1, errors)
      (0, errors)
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
  let errors =
    let add_edge errors (e : edge) =
      match Hashtbl.find_opt node_ht e.src, Hashtbl.find_opt node_ht e.dst with
      | Some (src, _), Some (dest, _) ->
        let l = { dest; delay = Distributions.constant 1. (* TODO read *) } in
        let n = nodes.(src) in
        nodes.(src) <- { n with links = l :: n.links };
        errors
      | _ -> "edge references missing node; omit edge" :: errors
    in
    List.fold_left
      (fun errors (e : edge) ->
        match graph.kind with
        | Directed -> add_edge errors e
        | Undirected ->
          let errors = add_edge errors e in
          add_edge errors { e with dst = e.src; src = e.dst })
      errors
      graph.edges
  in
  let net = { dissemination; nodes } in
  (* TODO, add second return value:
   * n : (int -> data) -> e : (int * int -> data) -> graph
   * that enables to get data back into the loaded graphml after simulation.
   *)
  match errors with
  | [] -> Ok net
  | _ -> Error (net, errors)
;;
