(** Persistent variant of {!Dag} *)

type 'a t
type vertex_id

val empty : unit -> 'a t
val size : 'a t -> int
val append : parents:vertex_id list -> 'data -> 'data t -> 'data t * vertex_id
val compare_vertex : vertex_id -> vertex_id -> int
val partial_order : vertex_id -> vertex_id -> int
val vertex_eq : vertex_id -> vertex_id -> bool
val vertex_neq : vertex_id -> vertex_id -> bool
val children : 'a t -> vertex_id -> vertex_id list
val parents : 'a t -> vertex_id -> vertex_id list
val data : 'a t -> vertex_id -> 'a
val update : vertex_id -> 'a -> 'a t -> 'a t
