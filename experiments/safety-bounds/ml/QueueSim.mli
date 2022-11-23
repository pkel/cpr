type time = T of float
type timedelta = D of float

type 'outcome step_outcome =
  | Stop of 'outcome
  | Continue

type ('event, 'outcome) model =
  { handler : (timedelta -> 'event -> unit) -> time -> 'event -> 'outcome step_outcome
  ; init : (time * 'event) list
  }

val run : ('event, 'outcome) model -> ('outcome, [ `EmptyQueue ]) result
