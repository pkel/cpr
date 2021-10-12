type 'a iid = unit -> 'a

val constant : 'a -> 'a iid
val uniform : lower:float -> upper:float -> float iid
val exponential : ev:float -> float iid

(** Optimized for repeated sampling with alias method *)
val discrete : weights:float list -> int iid

val float_of_string : string -> float iid StrResult.t

(** same as [float_of_string] but reuses previously returned values *)
val float_of_string_memoize : unit -> string -> float iid StrResult.t
