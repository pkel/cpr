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

(* Assumes that DAG is a tree and uses only the first parent *)
val common_ancestor : 'a view -> 'a node -> 'a node -> 'a node option

(* Assumes that DAG is a tree and uses only the first parent *)
val have_common_ancestor : 'a view -> 'a node -> 'a node -> bool

(* Assumes that DAG is a tree and uses only the first parent *)
val common_ancestor' : 'a view -> 'a node Seq.t -> 'a node option

(* Iterate backwards in DAG from given node (inclusive) to root. If a node has multiple
   parents, only the first parent is considered. *)
val seq_history : 'a view -> 'a node -> 'a node Seq.t

(** Print nodes and all descendants in graphviz dot format *)
val dot
  :  out_channel
  -> ?legend:(string * string) list
  -> 'a view
  -> node_attr:('a node -> (string * string) list)
  -> 'a node list
  -> unit
