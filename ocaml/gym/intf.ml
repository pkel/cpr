(* TODO consider using ocaml class for gym env interface *)
type 'instance env =
  { n_actions : int
  ; observation_length : int
  ; create : unit -> 'instance
  ; reset : 'instance -> floatarray
  ; step :
      'instance -> action:int -> floatarray * float * bool * (string * Py.Object.t) list
  ; to_string : 'instance -> string
  ; low : floatarray
  ; high : floatarray
  ; policies : (string * (floatarray -> int)) list
  }
