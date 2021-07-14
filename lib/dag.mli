(* mutable dag *)

type 'a t
type 'a node

(** maintenance *)

val create : unit -> 'a t
val roots : 'a t -> 'a node list
val append : 'a t -> 'a node list -> 'a -> 'a node

(** number of nodes *)
val size : 'a t -> int

(** data access *)
val data : 'a node -> 'a

(** node equality *)
val node_eq : 'a node -> 'a node -> bool

(** [id n] ranges from [0] to [size t - 1] *)
val id : 'a node -> int

(** views can restrict visibility of nodes; views cannot be edited *)
type 'a view

val view : 'a t -> 'a view

(** restrict visibility of nodes *)
val filter : ('a node -> bool) -> 'a view -> 'a view

(* local navigation *)

val parents : 'a view -> 'a node -> 'a node list
val children : 'a view -> 'a node -> 'a node list

(* advanced access *)

val leaves : 'a view -> 'a node -> 'a node list

(* TODO common_ancestor should support any DAG not only trees *)

(* Assumes that DAG is a tree and uses only the first parent *)
val common_ancestor : 'a view -> 'a node -> 'a node -> 'a node option

(* Assumes that DAG is a tree and uses only the first parent *)
val have_common_ancestor : 'a view -> 'a node -> 'a node -> bool

(* Assumes that DAG is a tree and uses only the first parent *)
val common_ancestor' : 'a view -> 'a node Seq.t -> 'a node option

(** [iterate_descendants v nodes] recursively expands the DAG in direction of {!children}
    ordered by depth and id. The starting nodes are included in the resulting sequence. *)
val iterate_descendants : 'a view -> 'a node list -> 'a node Seq.t

(** [iterate_ancestors v nodes] recursively expands the DAG in direction of {!parents}
    ordered by depth and id. The starting nodes are included in the resulting sequence. *)
val iterate_ancestors : 'a view -> 'a node list -> 'a node Seq.t

(** Print nodes and all descendants in graphviz dot format *)
val dot
  :  out_channel
  -> ?legend:(string * string) list
  -> 'a view
  -> node_attr:('a node -> (string * string) list)
  -> 'a node list
  -> unit
