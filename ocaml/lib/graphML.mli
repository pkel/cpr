(** GraphML: Writing and reading GraphML files targeting iGraph compatibility. *)

open Rresult

module Data : sig
  type value =
    | String of string
    | Float of float
    | Bool of bool

  type t = (string * value) list

  module Read : sig
    val string : value -> (string, R.msg) result
    val float : value -> (float, R.msg) result
    val bool : value -> (bool, R.msg) result
    val get : (value -> ('a, R.msg) result) -> string -> t -> ('a, R.msg) result
    val pop : (value -> ('a, R.msg) result) -> string -> t -> ('a * t, R.msg) result
  end

  module Write : sig
    val string : string -> value
    val float : float -> value
    val bool : bool -> value
    val int : int -> value
    val set : ('a -> value) -> string -> 'a -> t -> t
  end
end

type kind =
  | Directed
  | Undirected

type edge =
  { src : int
  ; dst : int
  ; data : Data.t
  }

type node =
  { id : int
  ; data : Data.t
  }

type graph =
  { kind : kind
  ; data : Data.t
  ; nodes : node list
  ; edges : edge list
  }

val show_graph : graph -> string
val pp_graph : Format.formatter -> graph -> unit
val graph_to_xml : graph -> (Ezxmlm.node, R.msg) result
val graph_of_xml : Ezxmlm.nodes -> (graph, R.msg) result
val load_graph : Fpath.t -> (graph, R.msg) result
val write_graph : graph -> Fpath.t -> (unit, R.msg) result
