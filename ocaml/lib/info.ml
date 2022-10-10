type value =
  | Bool of bool
  | Float of float
  | Int of int
  | String of string

let string_of_value = function
  | Bool x -> string_of_bool x
  | Float x -> string_of_float x
  | Int x -> string_of_int x
  | String x -> x
;;

let bool k x = k, Bool x
let float k x = k, Float x
let int k x = k, Int x
let string k x = k, String x

type t = (string * value) list
