(** GraphML: Writing and reading GraphML files targeting iGraph compatibility. *)

module Data : sig
  type value =
    | String of string
    | Float of float
    | Bool of bool

  type t = (string * value) list

  module Read : sig
    val string : value -> string StrResult.t
    val float : value -> float StrResult.t
    val bool : value -> bool StrResult.t
    val get : (value -> 'a StrResult.t) -> string -> t -> 'a StrResult.t
    val pop : (value -> 'a StrResult.t) -> string -> t -> ('a * t) StrResult.t
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
val graph_to_xml : graph -> (Ezxmlm.node, string) result
val graph_of_xml : Ezxmlm.nodes -> (graph, string) result
val load_graph : Fpath.t -> graph StrResult.t
val write_graph : graph -> Fpath.t -> unit StrResult.t
