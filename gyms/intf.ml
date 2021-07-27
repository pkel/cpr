type 'observation env =
  | Env :
      { n_actions : int
      ; create : unit -> 'instance
      ; reset : 'instance -> 'observation
      ; step : 'instance -> action:int -> 'observation * float * bool
      }
      -> 'observation env

(* TODO: provide code for registering a gym in a python environment *)
