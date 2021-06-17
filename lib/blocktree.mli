(* mutable block tree *)

type 'a t
type 'a block

(* maintenance *)
val create: 'a -> 'a t * 'a block
val append: 'a t -> 'a block -> 'a -> 'a block

(* partial visibility of blocks ; views cannot be edited *)
type ('a, 'b) view
val global_view: 'a t -> ('a, 'a) view
val local_view: ('a -> bool) -> ('a -> 'b) -> 'a t -> ('a, 'b) view

exception Invalid_block_argument
(** Raised by functions that operate on view and block when the block is not
  * visible in view. *)

(* data access *)

val data: ('a, 'b) view -> 'a block -> 'b
(** Raises {Invalid_block_argument}. *)

(* local navigation *)

val parent: ('a, 'b) view -> 'a block -> 'a block option
(** Raises {Invalid_block_argument}. *)

val children: ('a, 'b) view -> 'a block -> 'a block list
(** Raises {Invalid_block_argument}. *)
