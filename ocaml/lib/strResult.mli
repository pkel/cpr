type 'a t = ('a, string) result

val map : ('a -> 'b) -> 'a t -> 'b t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val errf : ('r, unit, string, _ t) Stdlib.format4 -> 'r
val get_exn : 'a t -> 'a

(* TODO have a look at Rresult.R. Can we use it? *)

module Syntax : sig
  (* infix *)
  val ( >|= ) : ('a, 'c) result -> ('a -> 'b) -> ('b, 'c) result
  val ( >>= ) : ('a, 'c) result -> ('a -> ('b, 'c) result) -> ('b, 'c) result

  (* applicative *)
  val ( let+ ) : ('a, 'c) result -> ('a -> 'b) -> ('b, 'c) result
  val ( and+ ) : ('a, 'c) result -> ('b, 'c) result -> ('a * 'b, 'c) result

  (* monad *)
  val ( let* ) : ('a, 'c) result -> ('a -> ('b, 'c) result) -> ('b, 'c) result
  val ( and* ) : ('a, 'c) result -> ('b, 'c) result -> ('a * 'b, 'c) result
end
