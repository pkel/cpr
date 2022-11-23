open Rresult

type 'a iid

val sample : 'a iid -> 'a
val constant : float -> float iid
val uniform : lower:float -> upper:float -> float iid
val exponential : ev:float -> float iid
val geometric : success_probability:float -> int iid

(** Optimized for repeated sampling with alias method *)
val discrete : weights:float list -> int iid

val to_string : 'a iid -> string
val float_of_string : string -> (float iid, R.msg) result

(** same as [float_of_string] but reuses previously returned values *)
val float_of_string_memoize : unit -> string -> (float iid, R.msg) result
