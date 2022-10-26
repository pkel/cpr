module Distributions = Cpr_lib.Distributions
open QueueSim

type v0_state =
  { mutable attacker : int (* height longest attacker chain *)
  ; mutable defender : int (* height longest defender chain *)
  ; mutable last_defender_block : float
  ; mutable tx : [ `NotIncluded | `IncludedAt of int | `Committed ]
  }

(* All tailgaters can be stolen by the attacker. This reflects the "rigged model" in the
   GR22AFT paper. *)
let version0 ~k ~cutoff ~tau ~lambda ~alpha ~delta : _ QueueSim.model =
  let mining_delay = Distributions.exponential ~ev:(1. /. lambda)
  and unif1 = Distributions.uniform ~lower:0. ~upper:1.
  and sample = Distributions.sample in
  let state =
    { attacker = 0
    ; defender = 0
    ; last_defender_block = Float.neg_infinity
    ; tx = `NotIncluded
    }
  in
  let handler schedule (T now) event =
    (* attacker can adopt latest defender chain until target tx is included *)
    if state.tx = `NotIncluded then state.attacker <- max state.attacker state.defender;
    (* mining & communication *)
    (match event with
    | `ProofOfWork when sample unif1 <= alpha ->
      (* attacker mines a block *)
      state.attacker <- state.attacker + 1;
      (* mining continues *)
      schedule (D (sample mining_delay)) `ProofOfWork
    | `ProofOfWork (* when unif1 > alpha *) ->
      (* defender mines a block *)
      if now -. state.last_defender_block > delta
      then (
        (* new block is not a tailgater *)
        state.defender <- state.defender + 1;
        state.last_defender_block <- now;
        (* include tx if available; commit if enough confirmations *)
        match state.tx with
        | `NotIncluded when now >= tau -> state.tx <- `IncludedAt state.defender
        | `IncludedAt h when state.defender >= h + k -> state.tx <- `Committed
        | _default -> ())
      else
        (* new block is tailgater; attacker steals *)
        state.attacker <- state.attacker + 1;
      (* mining continues *)
      schedule (D (sample mining_delay)) `ProofOfWork);
    (* termination *)
    if state.tx = `Committed
    then
      if state.attacker >= state.defender
      then Stop `Success
      else if state.defender - state.attacker > cutoff
      then (
        (* Probability of ever catching up is geometrically distributed. With rigged
           mining rate as in GR22AFT paper. Attacker can steal all tailgaters. *)
        let p_succ =
          let open GR22AFT in
          let p = p { lambda; delta; roh = 1. -. alpha; k } in
          t2F1 (state.defender - state.attacker) p
        in
        if sample unif1 <= p_succ then Stop `Success else Stop `Fail)
      else Continue
    else Continue
  and init = [ T (sample mining_delay), `ProofOfWork ] in
  { init; handler }
;;

type v1_state =
  { mutable attacker : int (* height longest attacker chain *)
  ; mutable defender_max : int (* height longest defender chain *)
  ; mutable defender_min : int
        (* height of longest defender chain available to all defenders *)
  ; mutable tx : [ `NotIncluded | `IncludedAt of int | `Committed ]
  }

(* No stealing is possible. This is clearly off, it is likely that some tailgaters can be
   stolen. Cool thing about this model is how it handles series of overlapping tailgaters
   accurately. *)
let version1 ~k ~cutoff ~tau ~lambda ~alpha ~delta : _ model =
  let defender_mining_delay = Distributions.exponential ~ev:(1. /. lambda /. (1. -. alpha))
  and attacker_mining_delay = Distributions.exponential ~ev:(1. /. lambda /. alpha)
  and unif1 = Distributions.uniform ~lower:0. ~upper:1.
  and sample = Distributions.sample in
  let state = { attacker = 0; defender_max = 0; defender_min = 0; tx = `NotIncluded } in
  let handler delay (T now) event =
    (* attacker can always adopt latest defender chain *)
    if state.tx = `NotIncluded
    then state.attacker <- max state.attacker state.defender_max;
    (* mining & communication *)
    (match event with
    | `Mining `Attacker ->
      (* attacker grows private chain *)
      state.attacker <- state.attacker + 1;
      (* mining continues unconditionally *)
      delay (D (sample attacker_mining_delay)) (`Mining `Attacker)
    | `Mining `Defender ->
      (* worst case: defender mines on shortest chain *)
      let height = state.defender_min + 1 in
      state.defender_max <- max state.defender_max height;
      (* include target tx if available; commit if enough confirmations *)
      (match state.tx with
      | `NotIncluded when now >= tau -> state.tx <- `IncludedAt height
      | `IncludedAt h when height >= h + k -> state.tx <- `Committed
      | _default -> ());
      (* others learn about new block in the future *)
      delay (D delta) (`Sync height);
      (* mining continues unconditionally *)
      delay (D (sample defender_mining_delay)) (`Mining `Defender)
    | `Sync height -> state.defender_min <- max state.defender_min height);
    if state.tx = `Committed
    then
      if state.attacker > state.defender_min
      then Stop `Success
      else if state.defender_min - state.attacker > cutoff
      then (
        (* Probability of ever catching up is geometrically distributed. With rigged
           mining rate as in GR22AFT paper. Attacker can steal all tailgaters.

           TODO: drop tailgaiters, not steal them *)
        let p_succ =
          let open GR22AFT in
          let p = p { lambda; delta; roh = 1. -. alpha; k } in
          t2F1 (state.defender_min - state.attacker) p
        in
        if sample unif1 <= p_succ then Stop `Success else Stop `Fail)
      else Continue
    else Continue
  and init =
    [ T (sample defender_mining_delay), `Mining `Defender
    ; T (sample attacker_mining_delay), `Mining `Attacker
    ]
  in
  { init; handler }
;;

type v2_state =
  { mutable attacker : int (* height longest attacker/alternative chain *)
  ; mutable defender_max : int (* height longest defender chain *)
  ; mutable defender_min : int
        (* height of longest defender chain available to all defenders *)
  ; mutable tx : [ `NotIncluded | `IncludedAt of int | `Committed ]
  ; mutable defenders : [ `Agree | `Disagree ]
  }

(* First attempt to allow stealing only when stealing is possible. *)
let version2 ~k ~cutoff ~tau ~lambda ~alpha ~delta : _ model =
  let mining_delay = Distributions.exponential ~ev:(1. /. lambda)
  and unif1 = Distributions.uniform ~lower:0. ~upper:1.
  and sample = Distributions.sample in
  let state =
    { attacker = 0
    ; defender_max = 0
    ; defender_min = 0
    ; tx = `NotIncluded
    ; defenders = `Disagree
    }
  in
  let handler delay (T now) event =
    (* adoption of defender chain *)
    if state.tx = `NotIncluded
    then
      (* no need to fork yet; attacker can adopt latest defender chain *)
      state.attacker <- max state.attacker state.defender_max;
    (* attacker might disturb synchrony of defenders *)
    if state.attacker > state.defender_min then state.defenders <- `Disagree;
    (* mining & communication *)
    (match event with
    | `ProofOfWork when sample unif1 <= alpha ->
      (* attacker grows private chain *)
      state.attacker <- state.attacker + 1;
      (* mining continues unconditionally *)
      delay (D (sample mining_delay)) `ProofOfWork
    | `ProofOfWork (* sample unif1 > alpha *) ->
      (* mining continues unconditionally *)
      let next_block = Distributions.sample mining_delay in
      delay (D next_block) `ProofOfWork;
      (* will defenders synchronize on unique preference? *)
      let unique =
        state.defender_min = state.defender_max
        && state.attacker <= state.defender_max
        && delta < next_block
      in
      (* worst case: defender mines on shortest chain *)
      let height = state.defender_min + 1 in
      (* include Tx if available; commit if enough confirmations *)
      (match state.tx with
      | `NotIncluded when now >= tau -> state.tx <- `IncludedAt height
      | `IncludedAt h when height >= h + k -> state.tx <- `Committed
      | _default -> ());
      (* longest chain is updated immediately *)
      state.defender_max <- max state.defender_max height;
      (* others learn about new block in the future *)
      delay (D delta) (`Sync (height, unique))
    | `Sync (height, unique) ->
      state.defender_min <- max state.defender_min height;
      if unique
      then state.defenders <- `Agree
      else if state.defenders = `Disagree
      then (* stealing *) state.attacker <- max state.attacker height);
    if state.tx = `Committed
    then
      if state.attacker > state.defender_min
      then Stop `Success
      else if state.defender_min - state.attacker > cutoff
      then (
        (* Probability of ever catching up is geometrically distributed. With rigged
           mining rate as in GR22AFT paper. Attacker can steal all tailgaters.

           TODO: drop tailgaiters, not steal them *)
        let p_succ =
          let open GR22AFT in
          let p = p { lambda; delta; roh = 1. -. alpha; k } in
          t2F1 (state.defender_min - state.attacker) p
        in
        if sample unif1 <= p_succ then Stop `Success else Stop `Fail)
      else Continue
    else Continue
  and init = [ T (sample mining_delay), `ProofOfWork ] in
  { init; handler }
;;

let test_run name model =
  let block_interval = 13. in
  let model () =
    model
      ~k:30
      ~cutoff:30
      ~tau:(200. *. block_interval)
      ~lambda:(1. /. block_interval)
      ~alpha:0.33
      ~delta:2.
  in
  Printf.printf "%s: " name;
  let n = 5000 in
  let i = ref n
  and fail, success, error = ref 0, ref 0, ref 0 in
  while !i > 0 do
    decr i;
    match run (model ()) with
    | Ok `Success -> incr success
    | Ok `Fail -> incr fail
    | Error `EmptyQueue -> incr error
  done;
  if !error > 0 then Printf.eprintf "WARNING: observed %d errors\n" !error;
  let p = float_of_int !success /. float_of_int n *. 100. in
  Printf.printf "%d trials, %d successes, success_rate: %g%%\n" n !success p
;;

let test () =
  Random.init 1;
  test_run "v0" version0;
  test_run "v0" version0;
  test_run "v0" version0;
  test_run "v1" version1;
  test_run "v1" version1;
  test_run "v1" version1;
  test_run "v2" version2;
  test_run "v2" version2;
  test_run "v2" version2
;;

let main () =
  Random.init 42;
  let delta = 1. in
  print_endline "block_interval,alpha,k,model,i,p";
  let row ~block_interval ~alpha ~k ~model ~i p =
    Printf.printf "%g,%g,%i,%s,%i,%g\n%!" block_interval alpha k model i p
  in
  List.iter
    (fun block_interval ->
      List.iter
        (fun alpha ->
          let lambda = 1. /. block_interval
          and tau = 200. *. block_interval in
          let versions ~k =
            let cutoff = 1 * k in
            [ ("v0", fun () -> run (version0 ~k ~cutoff ~lambda ~alpha ~delta ~tau))
            ; ("v1", fun () -> run (version1 ~k ~cutoff ~lambda ~alpha ~delta ~tau))
            ; ("v2", fun () -> run (version2 ~k ~cutoff ~lambda ~alpha ~delta ~tau))
            ]
          in
          for k = 1 to 20 do
            let () =
              let open GR22AFT in
              let p = { k; delta; lambda; roh = 1. -. alpha } in
              row ~block_interval ~alpha ~k ~model:"gr22t1lower" ~i:0 (t1lower p);
              row ~block_interval ~alpha ~k ~model:"gr22t1upper" ~i:0 (t1upper p);
              row ~block_interval ~alpha ~k ~model:"gr22t2lower" ~i:0 (t2lower p);
              row ~block_interval ~alpha ~k ~model:"gr22t2upper" ~i:0 (t2upper p)
            in
            List.iter
              (fun (model, job) ->
                for i = 1 to 10 (* observations per config *) do
                  (* do a few simulations, estimate success rate *)
                  let fail, success = ref 0, ref 0 in
                  let n = 1000 (* simulations per observation *) in
                  for _j = 1 to n do
                    match job () with
                    | Ok `Success -> incr success
                    | Ok `Fail -> incr fail
                    | Error `EmptyQueue -> failwith "empty queue"
                  done;
                  if !success > 0
                  then (
                    let p = float_of_int !success /. float_of_int n in
                    row ~block_interval ~alpha ~k ~model ~i p)
                done)
              (versions ~k)
          done)
        (* alphas *) [ 0.1; 0.25; 0.33 ])
    (* block intervals *) [ 4.; 8.; 16. ]
;;

let () =
  match Bos.OS.Env.req_var "TEST" with
  | Ok _ -> test ()
  | Error _ -> main ()
;;
