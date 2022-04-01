(* mutable dag *)

type 'a t
type 'a vertex

(** maintenance *)

val create : unit -> 'a t
val roots : 'a t -> 'a vertex list
val append : 'a t -> 'a vertex list -> 'a -> 'a vertex

(** number of vertices *)
val size : 'a t -> int

(** data access *)
val data : 'a vertex -> 'a

(** vertex equality *)
val vertex_eq : 'a vertex -> 'a vertex -> bool

val vertex_neq : 'a vertex -> 'a vertex -> bool

(** partial order. Only useful, if the compared vertices are on the same path. *)
val partial_order : 'a vertex -> 'a vertex -> int

(** [id n] ranges from [0] to [size t - 1] *)
val id : 'a vertex -> int

(** views can restrict visibility of vertices; views cannot be edited *)
type 'a view

val view : 'a t -> 'a view

(** restrict visibility of vertices *)
val filter : ('a vertex -> bool) -> 'a view -> 'a view

(* local navigation *)

val parents : 'a view -> 'a vertex -> 'a vertex list
val children : 'a view -> 'a vertex -> 'a vertex list

(* advanced access *)

val leaves : 'a view -> 'a vertex -> 'a vertex list
val common_ancestor : 'a view -> 'a vertex -> 'a vertex -> 'a vertex option
val have_common_ancestor : 'a view -> 'a vertex -> 'a vertex -> bool
val common_ancestor' : 'a view -> 'a vertex Seq.t -> 'a vertex option

(** [iterate_descendants v vertices] recursively expands the DAG in direction of
    {!children} ordered by depth and id. The starting vertices are included in the
    resulting sequence. Returned vertices are unique. *)
val iterate_descendants : 'a view -> 'a vertex list -> 'a vertex Seq.t

(** [iterate_ancestors v vertices] recursively expands the DAG in direction of {!parents}
    ordered by depth and id. The starting vertices are included in the resulting sequence.
    Returned vertices are unique. *)
val iterate_ancestors : 'a view -> 'a vertex list -> 'a vertex Seq.t

(** Print vertices and all descendants in graphviz dot format *)
val dot
  :  Format.formatter
  -> ?legend:(string * string) list
  -> 'a view
  -> node_attr:('a vertex -> (string * string) list)
  -> 'a vertex list
  -> unit

(** Export vertices and all descendants to iGraph/GraphML representation *)
val graphml : 'a view -> ('a -> GraphML.Data.t) -> 'a vertex list -> GraphML.graph

module Exn : sig
  type exn +=
    | Malformed_DAG of
        { msg : string
        ; dag : string lazy_t
        }

  val set_to_file : string -> unit
  val raise : 'a view -> ('a -> (string * string) list) -> 'a vertex list -> string -> 'b
end
