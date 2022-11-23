module OrderedQueue = Cpr_lib.OrderedQueue

type time = T of float
type timedelta = D of float

type 'outcome step_outcome =
  | Stop of 'outcome
  | Continue

type ('event, 'outcome) model =
  { handler : (timedelta -> 'event -> unit) -> time -> 'event -> 'outcome step_outcome
  ; init : (time * 'event) list
  }

type 'event sim_state =
  { mutable queue : (float, 'event) OrderedQueue.t
  ; mutable time : float
  }

let init events =
  let open OrderedQueue in
  let queue =
    let empty = init Float.compare in
    List.fold_left (fun acc (T time, event) -> queue time event acc) empty events
  in
  { queue; time = 0. }
;;

let run model =
  let state = init model.init in
  let delay (D d) ev =
    state.queue <- OrderedQueue.queue (state.time +. d) ev state.queue
  in
  let rec step () =
    match OrderedQueue.dequeue state.queue with
    | None -> Error `EmptyQueue
    | Some (t, e, q) ->
      state.time <- t;
      state.queue <- q;
      (match model.handler delay (T t) e with
      | Continue -> step ()
      | Stop outcome -> Ok outcome)
  in
  step ()
;;
