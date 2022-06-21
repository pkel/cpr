(* OCaml gym <> Python gym interface *)
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
  ; puzzles_per_block : int
  }

module Sim = struct
  type state = S of int
  type message = M of unit

  type event =
    | A of int
    | B of int

  type action = bool

  let init = ()
  let next_event (S _) = if Random.bool () then A (Random.int 42) else B (Random.int 42)

  let step state event =
    let (S x) = state in
    match event with
    | A y -> S (x - y)
    | B y -> S (x + y)
  ;;

  let rec loop n state =
    if n <= 0 then state else loop (n - 1) (step state (next_event state))
  ;;

  type stopped_sim =
    { current_event : event
    ; continue : process_event:bool -> share:(int * message) list -> stopped_sim
    ; shutdown : process_event:bool -> share:(int * message) list -> unit
    }

  let rec continue ~till state =
    let ev = next_event state in
    if not (till ev)
    then continue ~till (step state ev)
    else
      { current_event = ev
      ; continue =
          (fun ~process_event ~share:_ ->
            if process_event then continue ~till (step state ev) else continue ~till state)
      ; shutdown = (fun ~process_event:_ ~share:_ -> ())
      }
  ;;
end

module Gym = struct
  type state =
    { a : int
    ; sim : Sim.stopped_sim
    }

  let step state =
    let open Sim in
    match state.sim.current_event with
    | A x -> { a = state.a + x; sim = state.sim.continue ~process_event:true ~share:[] }
    | B _ -> { state with sim = state.sim.continue ~process_event:true ~share:[] }
  ;;
end
