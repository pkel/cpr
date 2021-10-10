(** {1} GraphML Writing and reading GraphML files targeting iGraph compatibility. *)

module Data : sig
  type value =
    | String of string
    | Double of float
    | Boolean of bool

  type t = (string * value) list

  module Read : sig
    type 'a f = string -> t -> ('a, [ `Key_not_found | `Type_mismatch ]) result

    val string : string f
    val double : float f
    val boolean : bool f
  end

  module Pop : sig
    type 'a f = string -> t -> ('a * t, [ `Key_not_found | `Type_mismatch ]) result

    val string : string f
    val double : float f
    val boolean : bool f
  end

  module Set : sig
    type 'a f = string -> 'a -> t -> t

    val string : string f
    val double : float f
    val boolean : bool f
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
