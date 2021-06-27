(* mutable dag *)

type 'a t
type 'a node

(** maintenance *)

val roots : 'a list -> 'a t * 'a node list
val append : 'a t -> 'a node list -> 'a -> 'a node

(* TODO: what if parents are empty? *)

(** data access *)
val data : 'a node -> 'a

(** views can restrict visibility of nodes; views cannot be edited *)
type 'a view

val view : 'a t -> 'a view

(** restrict visibility of nodes *)
val filter : ('a -> bool) -> 'a view -> 'a view

(* local navigation *)

val parents : 'a view -> 'a node -> 'a node list
val children : 'a view -> 'a node -> 'a node list

(* advanced access *)

val leaves : 'a view -> 'a node -> 'a node list

(* Currently works only on single ancestor DAGs, i.e. trees. *)
val common_ancestor : 'a view -> 'a node -> 'a node -> 'a node option

(* Currently works only on single ancestor DAGs, i.e. trees. *)
val have_common_ancestor : 'a view -> 'a node -> 'a node -> bool

(* Currently works only on single ancestor DAGs, i.e. trees. *)
val common_ancestor' : 'a view -> 'a node Seq.t -> 'a node option
