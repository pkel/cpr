type 'a iid = unit -> 'a

val constant : 'a -> 'a iid
val uniform : lower:float -> upper:float -> float iid
val exponential : ev:float -> float iid

(** Optimized for repeated sampling with alias method *)
val discrete : weights:float list -> int iid
