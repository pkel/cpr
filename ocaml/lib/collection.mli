type 'a t

type 'a entry =
  { key : string
  ; info : string
  ; it : 'a
  }

val all : 'a t -> string list
val get : string -> 'a t -> 'a entry option
val empty : 'a t
val add : info:string -> string -> 'a -> 'a t -> 'a t
val iter : ('a entry -> unit) -> 'a t -> unit
val map : ('a entry -> 'b entry) -> 'a t -> 'b t
val map_to_list : ('a entry -> 'b) -> 'a t -> 'b list
