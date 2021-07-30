type ('instance, 'observation) env =
  { n_actions : int
  ; create : unit -> 'instance
  ; reset : 'instance -> 'observation
  ; step : 'instance -> action:int -> 'observation * float * bool
  ; to_string : 'instance -> string
  ; numpy : 'observation -> floatarray
  ; low : floatarray
  ; high : floatarray
  }

(* TODO: provide code for registering a gym in a python environment *)
