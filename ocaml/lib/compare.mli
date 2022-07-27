type 'a cmp = 'a -> 'a -> int

val bool : bool cmp
val int : int cmp
val float : float cmp
val neg : 'a cmp -> 'a cmp
val tuple : 'a cmp -> 'b cmp -> ('a * 'b) cmp
val by : 'a cmp -> ('b -> 'a) -> 'b cmp

(** [disambiguate cmp0 cmp1] disambiguates [cmp0 a b = 0] using [cmp1]. The latter is
    evaluated lazily. *)
val disambiguate : 'a cmp -> 'a cmp -> 'a cmp

(** infix operator for {!disambiguate} *)
val ( $ ) : 'a cmp -> 'a cmp -> 'a cmp

(** Avoid expensive comparison when values are equal *)
val skip_eq : ('a -> 'a -> bool) -> 'a cmp -> 'a cmp

val is_sorted : ?unique:bool -> 'a cmp -> 'a list -> bool
val first : ?skip_to:('a -> bool) -> 'a cmp -> int -> 'a list -> 'a list option
val at_most_first : 'a cmp -> int -> 'a list -> 'a list
