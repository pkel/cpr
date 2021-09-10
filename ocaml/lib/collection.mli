type 'a t

val all : 'a t -> string list
val get : string -> 'a t -> 'a option
val describe : string -> 'a t -> string option
val empty : 'a t
val add : info:string -> string -> 'a -> 'a t -> 'a t
val iter : (info:string -> string -> 'a -> unit) -> 'a t -> unit
val map_to_list : (info:string -> string -> 'a -> 'b) -> 'a t -> 'b list
