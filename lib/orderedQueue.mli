type ('a, 'b) t

val length: ('a, 'b) t -> int

val init : ('a -> 'a -> int) -> ('a, 'b) t
val queue : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t

val dequeue : ('a, 'b) t -> ('a * 'b * ('a, 'b) t) option
