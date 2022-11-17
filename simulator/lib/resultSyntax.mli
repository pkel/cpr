(* applicative *)
val ( let+ ) : ('a, 'c) result -> ('a -> 'b) -> ('b, 'c) result
val ( and+ ) : ('a, 'c) result -> ('b, 'c) result -> ('a * 'b, 'c) result

(* monad *)
val ( let* ) : ('a, 'c) result -> ('a -> ('b, 'c) result) -> ('b, 'c) result
val ( and* ) : ('a, 'c) result -> ('b, 'c) result -> ('a * 'b, 'c) result
