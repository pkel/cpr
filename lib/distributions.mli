type 'a iid = unit -> 'a

val exponential : ev:float -> float iid

(** Optimized for repeated sampling with alias method *)
val discrete : weights:float list -> int iid
