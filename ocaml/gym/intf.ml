type 'instance env =
  { n_actions : int
  ; create : unit -> 'instance
  ; reset : 'instance -> floatarray
  ; step :
      'instance -> action:int -> floatarray * float * bool * (string * Py.Object.t) list
  ; to_string : 'instance -> string
  ; low : floatarray
  ; high : floatarray
  }

(* TODO: provide code for registering a gym in a python environment *)
