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

(** Fast, physical equality. *)
val vertex_eq : 'a vertex -> 'a vertex -> bool

(** Fast, physical inequality. *)
val vertex_neq : 'a vertex -> 'a vertex -> bool

(** partial order. Only useful, if the compared vertices are on the same path.
    Parents are smaller. *)
val partial_order : 'a vertex -> 'a vertex -> int

(** [id n] ranges from [0] to [size t - 1] *)
val id : 'a vertex -> int

(** Full order. Compares vertices by [depth x, id x].
    Compatible with {!partial_order}.
    Parents are smaller. *)
val compare_vertex : 'a vertex -> 'a vertex -> int

(** views can restrict visibility of vertices; views cannot be edited *)
type 'a view

val view : 'a t -> 'a view

(** restrict visibility of vertices *)
val filter : ('a vertex -> bool) -> 'a view -> 'a view

(* local navigation *)

val parents : 'a view -> 'a vertex -> 'a vertex list
val children : 'a view -> 'a vertex -> 'a vertex list
