(* mutable dag *)

type 'a t
type 'a node

(** maintenance *)

val roots : 'a list -> 'a t * 'a node list
val append : 'a t -> 'a node list -> 'a -> 'a node

(** data access *)
val data : 'a node -> 'a

(** views can restrict visibility of nodes; views cannot be edited *)
type 'a view

val view : 'a t -> 'a view

(** restrict visibility of nodes *)
val filter : ('a -> bool) -> 'a view -> 'a view

(** Raised by view functions view when the node argument is not visible. *)
exception Invalid_node_argument

(* local navigation *)

(** Raises {Invalid_node_argument}. *)
val parents : 'a view -> 'a node -> 'a node list

(** Raises {Invalid_node_argument}. *)
val children : 'a view -> 'a node -> 'a node list
