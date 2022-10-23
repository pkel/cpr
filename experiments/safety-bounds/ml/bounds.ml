module Distributions = Cpr_lib.Distributions
module OrderedQueue = Cpr_lib.OrderedQueue

type event =
  | Mining of [ `Attacker | `Defender ]
  | Sync of { height : int }

type state =
  { mutable attacker : int (* height longest attacker chain *)
  ; mutable defender_max : int (* height longest defender chain *)
  ; mutable defender_min : int
        (* height longest defender chain available to all defenders *)
  ; mutable tx : [ `NotIncluded | `IncludedAt of int | `Committed ]
  }

type attack_outcome =
  | Aborted
  | Success

module Simulation : sig
  type time = T of float
  type timedelta = D of float

  type step_outcome =
    | Stop of attack_outcome
    | Continue

  type model =
    { handler : (timedelta -> event -> unit) -> time -> event -> step_outcome
    ; init : (time * event) list
    }

  val run : model -> (attack_outcome, [ `EmptyQueue ]) result
end = struct
  type time = T of float
  type timedelta = D of float

  type step_outcome =
    | Stop of attack_outcome
    | Continue

  type model =
    { handler : (timedelta -> event -> unit) -> time -> event -> step_outcome
    ; init : (time * event) list
    }

  type sim_state =
    { mutable queue : (float, event) OrderedQueue.t
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
end

open Simulation

type network =
  { defender_mining_delay : float Distributions.iid
  ; attacker_mining_delay : float Distributions.iid
  ; propagation_delay : float
  }

let nakamoto ~k ~cutoff ~tau network : model =
  let state = { attacker = 0; defender_max = 0; defender_min = 0; tx = `NotIncluded } in
  let handler delay (T now) event =
    (* attacker can always adopt latest defender chain *)
    if state.tx = `NotIncluded
    then state.attacker <- max state.attacker state.defender_max;
    (* mining & communication *)
    (match event with
    | Mining `Attacker ->
      (* attacker grows private chain *)
      state.attacker <- state.attacker + 1;
      (* mining continues unconditionally *)
      delay (D (Distributions.sample network.attacker_mining_delay)) (Mining `Attacker)
    | Mining `Defender ->
      (* worst case: defender mines on shortest chain *)
      let height = state.defender_min + 1 in
      state.defender_max <- max state.defender_max height;
      (* include Tx if available; commit if enough confirmations *)
      (match state.tx with
      | `NotIncluded when now >= tau -> state.tx <- `IncludedAt height
      | `IncludedAt h when height >= h + k -> state.tx <- `Committed
      | _default -> ());
      (* others learn about new block in the future *)
      delay (D network.propagation_delay) (Sync { height });
      (* mining continues unconditionally *)
      delay (D (Distributions.sample network.defender_mining_delay)) (Mining `Defender)
    | Sync { height } -> state.defender_min <- max state.defender_min height);
    if state.tx = `Committed
    then
      if state.attacker > state.defender_min
      then Stop Success
      else if state.attacker < state.defender_min + cutoff
      then Stop Aborted
      else Continue
    else Continue
  and init =
    [ T (Distributions.sample network.defender_mining_delay), Mining `Defender
    ; T (Distributions.sample network.attacker_mining_delay), Mining `Attacker
    ]
  in
  { init; handler }
;;

let model () : model =
  let block_interval = 6.
  and alpha = 0.33 in
  let network =
    { defender_mining_delay =
        Distributions.exponential ~ev:(block_interval /. (1. -. alpha))
    ; attacker_mining_delay = Distributions.exponential ~ev:(block_interval /. alpha)
    ; propagation_delay = 1.
    }
  in
  nakamoto network ~k:6 ~cutoff:12 ~tau:(200. *. block_interval)
;;

let () =
  Random.self_init ();
  let n = 100000 in
  let i = ref n
  and abort, success, error = ref 0, ref 0, ref 0 in
  while !i > 0 do
    decr i;
    match run (model ()) with
    | Ok Success -> incr success
    | Ok Aborted -> incr abort
    | Error `EmptyQueue -> incr error
  done;
  if !error > 0 then Printf.eprintf "WARNING: observed %d errors\n" !error;
  let p = float_of_int !success /. float_of_int n *. 100. in
  Printf.printf "%d trials, %d successes, success_rate: %g%%\n" n !success p
;;
