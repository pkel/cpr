module Distributions = Cpr_lib.Distributions
open QueueSim

type v0_state =
  { mutable attacker : int (* height longest attacker chain *)
  ; mutable defender : int (* height longest defender chain *)
  ; mutable tx : [ `Pending | `IncludedAt of int | `Committed ]
  }

(* All tailgaters can be stolen by the attacker. This reflects the "rigged model" in the
   GR22AFT paper.

   GR give one additional block to the attacker due to analytical reasons. That's not
   necessary with this approach. For full complicate call this function with
   [~atk_plus:1]. *)
let version0 ~atk_plus ~k ~cutoff ~tau ~lambda ~alpha ~delta : _ QueueSim.model =
  let mining_delay = Distributions.exponential ~ev:(1. /. lambda)
  and unif1 = Distributions.uniform ~lower:0. ~upper:1.
  and sample = Distributions.sample in
  let sample_mining () =
    let delay = sample mining_delay in
    let a = if delay <= delta then `Tailgater else `Lagger
    and b = if sample unif1 <= alpha then `Attacker else `Defender in
    delay, (a, b)
  in
  let state = { attacker = 0; defender = 0; tx = `Pending } in
  let handler schedule (T now) event =
    let continue_mining () =
      let delay, event = sample_mining () in
      schedule (D delay) event
    in
    (* attacker can adopt latest defender chain until target tx is included *)
    if state.tx = `Pending then state.attacker <- max state.attacker state.defender;
    (* mining & communication *)
    (match event with
    | _, `Attacker | `Tailgater, `Defender ->
      state.attacker <- state.attacker + 1;
      continue_mining ()
    | `Lagger, `Defender ->
      state.defender <- state.defender + 1;
      (* include tx if available; commit if enough confirmations *)
      (match state.tx with
      | `Pending when now >= tau -> state.tx <- `IncludedAt state.defender
      | `IncludedAt h when state.defender >= h + k -> state.tx <- `Committed
      | _default -> ());
      continue_mining ());
    if state.tx = `Committed
    then
      if state.attacker >= state.defender
      then Stop `Success
      else if state.defender - state.attacker > cutoff
      then (
        (* Probability of ever catching up is geometrically distributed with rigged mining
           rate as in GR22AFT paper. Attacker can steal all tailgaters. *)
        let p_succ =
          let open GR22AFT in
          let p = p { lambda; delta; rho = 1. -. alpha; k } in
          t2F1 (state.defender - state.attacker - atk_plus) p
        in
        if sample unif1 <= p_succ then Stop `Success else Stop `Fail)
      else Continue
    else Continue
  and init =
    let delay, event = sample_mining () in
    [ T delay, event ]
  in
  { init; handler }
;;

let version0_compliant = version0 ~atk_plus:1
let version0 = version0 ~atk_plus:0

(* like version0 but instead of doing the pre-mining explicitly we sample the initial
   state from the pre-mining distribution. TODO *)
let version0_pre ~atk_plus ~k ~cutoff ~tau ~lambda ~alpha ~delta : _ QueueSim.model =
  let mining_delay = Distributions.exponential ~ev:(1. /. lambda)
  and unif1 = Distributions.uniform ~lower:0. ~upper:1.
  and mod_geom =
    let success_probability =
      let p = (1. -. alpha) *. exp (-1. *. lambda *. delta) in
      ((2. *. p) -. 1.) /. p
    in
    Distributions.geometric ~success_probability
  and sample = Distributions.sample in
  let sample_mining () =
    let delay = sample mining_delay in
    let a = if delay <= delta then `Tailgater else `Lagger
    and b = if sample unif1 <= alpha then `Attacker else `Defender in
    delay, (a, b)
  in
  let state = { attacker = 0; defender = 0; tx = `Pending } in
  let handler schedule (T now) event =
    let continue_mining () =
      let delay, event = sample_mining () in
      schedule (D delay) event
    in
    (* attacker can adopt latest defender chain until target tx is included *)
    if state.tx = `Pending then state.attacker <- max state.attacker state.defender;
    (* mining & communication *)
    (match event with
    | _, `Attacker | `Tailgater, `Defender ->
      state.attacker <- state.attacker + 1;
      continue_mining ()
    | `Lagger, `Defender ->
      state.defender <- state.defender + 1;
      (* include tx if available; commit if enough confirmations *)
      (match state.tx with
      | `Pending when now >= tau -> state.tx <- `IncludedAt state.defender
      | `IncludedAt h when state.defender >= h + k -> state.tx <- `Committed
      | _default -> ());
      continue_mining ());
    if state.tx = `Committed
    then
      if state.attacker >= state.defender
      then Stop `Success
      else if state.defender - state.attacker > cutoff
      then
        if (* Probability of ever catching up is geometrically distributed with rigged
              mining rate as in GR22AFT paper. Attacker can steal all tailgaters. *)
           sample mod_geom >= state.defender - state.attacker - atk_plus
        then Stop `Success
        else Stop `Fail
      else Continue
    else Continue
  and init =
    let delay, event = sample_mining () in
    [ T delay, event ]
  in
  { init; handler }
;;

let prob_defender_given_not_dropped ~alpha ~lambda ~delta =
  let rho = 1. -. alpha in
  let p = rho *. Float.exp (-1. *. lambda *. delta) in
  p /. (alpha +. p)
;;

(* All tailgaters are dropped. *)
let version1 ~k ~cutoff ~tau ~lambda ~alpha ~delta : _ QueueSim.model =
  let mining_delay = Distributions.exponential ~ev:(1. /. lambda)
  and unif1 = Distributions.uniform ~lower:0. ~upper:1.
  and sample = Distributions.sample in
  let sample_mining () =
    let delay = sample mining_delay in
    let a = if delay <= delta then `Tailgater else `Lagger
    and b = if sample unif1 <= alpha then `Attacker else `Defender in
    delay, (a, b)
  in
  let state = { attacker = 0; defender = 0; tx = `Pending } in
  let handler schedule (T now) event =
    let continue_mining () =
      let delay, event = sample_mining () in
      schedule (D delay) event
    in
    (* attacker can adopt latest defender chain until target tx is included *)
    if state.tx = `Pending then state.attacker <- max state.attacker state.defender;
    (* mining & communication *)
    (match event with
    | _, `Attacker ->
      state.attacker <- state.attacker + 1;
      continue_mining ()
    | `Tailgater, `Defender -> continue_mining ()
    | `Lagger, `Defender ->
      state.defender <- state.defender + 1;
      (* include tx if available; commit if enough confirmations *)
      (match state.tx with
      | `Pending when now >= tau -> state.tx <- `IncludedAt state.defender
      | `IncludedAt h when state.defender >= h + k -> state.tx <- `Committed
      | _default -> ());
      continue_mining ());
    if state.tx = `Committed
    then
      if state.attacker >= state.defender
      then Stop `Success
      else if state.defender - state.attacker > cutoff
      then (
        (* Probability of ever catching up is geometrically distributed. Here, attacker
           cannot steal tailgaters. *)
        let p_succ =
          let open GR22AFT in
          let p = prob_defender_given_not_dropped ~lambda ~delta ~alpha in
          t2F1 (state.defender - state.attacker) p
        in
        if sample unif1 <= p_succ then Stop `Success else Stop `Fail)
      else Continue
    else Continue
  and init =
    let delay, event = sample_mining () in
    [ T delay, event ]
  in
  { init; handler }
;;

type v1_state =
  { mutable attacker : int (* height longest attacker chain *)
  ; mutable defender_max : int (* height longest defender chain *)
  ; mutable defender_min : int
        (* height of longest defender chain available to all defenders *)
  ; mutable tx : [ `Pending | `IncludedAt of int | `Committed ]
  }

(* No stealing. Explicit communication *)
let version2 ~k ~cutoff ~tau ~lambda ~alpha ~delta : _ model =
  let defender_mining_delay = Distributions.exponential ~ev:(1. /. lambda /. (1. -. alpha))
  and attacker_mining_delay = Distributions.exponential ~ev:(1. /. lambda /. alpha)
  and unif1 = Distributions.uniform ~lower:0. ~upper:1.
  and sample = Distributions.sample in
  let state = { attacker = 0; defender_max = 0; defender_min = 0; tx = `Pending } in
  let handler schedule (T now) event =
    (* attacker can always adopt latest defender chain *)
    if state.tx = `Pending then state.attacker <- max state.attacker state.defender_max;
    (* mining & communication *)
    (match event with
    | `Mining `Attacker ->
      (* attacker grows private chain *)
      state.attacker <- state.attacker + 1;
      (* mining continues unconditionally *)
      schedule (D (sample attacker_mining_delay)) (`Mining `Attacker)
    | `Mining `Defender ->
      (* worst case: defender mines on shortest chain *)
      let height = state.defender_min + 1 in
      state.defender_max <- max state.defender_max height;
      (* include target tx if available; commit if enough confirmations *)
      (match state.tx with
      | `Pending when now >= tau -> state.tx <- `IncludedAt height
      | `IncludedAt h when height >= h + k -> state.tx <- `Committed
      | _default -> ());
      (* others learn about new block in the future *)
      schedule (D delta) (`Rx height);
      (* mining continues unconditionally *)
      schedule (D (sample defender_mining_delay)) (`Mining `Defender)
    | `Rx height -> state.defender_min <- max state.defender_min height);
    if state.tx = `Committed
    then
      if state.attacker > state.defender_min
      then Stop `Success
      else if state.defender_min - state.attacker > cutoff
      then (
        (* Probability of ever catching up is geometrically distributed. Tailgaters are
           dropped. *)
        let p_succ =
          let open GR22AFT in
          let p = prob_defender_given_not_dropped ~lambda ~delta ~alpha in
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

type v3_state =
  { mutable attacker : int (* height longest attacker/alternative chain *)
  ; mutable defender_max : int (* height longest defender chain *)
  ; mutable defender_min : int
        (* height of longest defender chain available to all defenders *)
  ; mutable tx : [ `Pending | `IncludedAt of int | `Committed ]
  ; mutable stealing : bool
  }

(* An attempt to allow stealing only when stealing is possible. *)
let version3 ~k ~cutoff ~tau ~lambda ~alpha ~delta : _ model =
  let mining_delay = Distributions.exponential ~ev:(1. /. lambda)
  and unif1 = Distributions.uniform ~lower:0. ~upper:1.
  and sample = Distributions.sample in
  let state =
    { attacker = 0; defender_max = 0; defender_min = 0; tx = `Pending; stealing = true }
  in
  let handler schedule (T now) event =
    (* adoption of defender chain *)
    if state.tx = `Pending
    then
      (* no need to fork yet; attacker can adopt latest defender chain *)
      state.attacker <- max state.attacker state.defender_max;
    (* stealing becomes possible *)
    if state.attacker > state.defender_min then state.stealing <- true;
    (* mining & communication *)
    (match event with
    | `ProofOfWork when sample unif1 <= alpha ->
      (* attacker grows private chain *)
      state.attacker <- state.attacker + 1;
      (* mining continues unconditionally *)
      schedule (D (sample mining_delay)) `ProofOfWork
    | `ProofOfWork (* sample unif1 > alpha *) ->
      (* mining continues unconditionally *)
      let delay = Distributions.sample mining_delay in
      schedule (D delay) `ProofOfWork;
      (* will defenders synchronize on unique preference? *)
      if now > tau
         && state.attacker <= state.defender_max
         && state.defender_min = state.defender_max
         && delay > delta
      then state.stealing <- false;
      (* worst case: defender mines on shortest chain *)
      let height = state.defender_min + 1 in
      (* include Tx if available; commit if enough confirmations *)
      (match state.tx with
      | `Pending when now >= tau -> state.tx <- `IncludedAt height
      | `IncludedAt h when height >= h + k -> state.tx <- `Committed
      | _default -> ());
      (* longest chain is updated immediately *)
      state.defender_max <- max state.defender_max height;
      if state.stealing then state.attacker <- max state.attacker height;
      (* others learn about new block in the future *)
      schedule (D delta) (`Rx height)
    | `Rx height -> state.defender_min <- max state.defender_min height);
    if state.tx = `Committed
    then
      if state.attacker > state.defender_min
      then Stop `Success
      else if state.defender_min - state.attacker > cutoff
      then (
        (* Probability of ever catching up is geometrically distributed. Tailgaters cannot
           be stolen. *)
        let p_succ =
          let open GR22AFT in
          let p = prob_defender_given_not_dropped ~alpha ~lambda ~delta in
          t2F1 (state.defender_min - state.attacker) p
        in
        if sample unif1 <= p_succ then Stop `Success else Stop `Fail)
      else Continue
    else Continue
  and init = [ T (sample mining_delay), `ProofOfWork ] in
  { init; handler }
;;

let test_run name model =
  (* Ethereum config *)
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
  Printf.printf "%d trials, %4d successes, success_rate: %g%%\n" n !success p
;;

let test () =
  Random.init 1;
  test_run "v0" version0;
  test_run "v0" version0;
  test_run "v0" version0;
  test_run "v0c" version0_compliant;
  test_run "v0c" version0_compliant;
  test_run "v0c" version0_compliant;
  test_run "v0_pre" (version0_pre ~atk_plus:0);
  test_run "v0_pre" (version0_pre ~atk_plus:0);
  test_run "v0_pre" (version0_pre ~atk_plus:0);
  test_run "v1" version1;
  test_run "v1" version1;
  test_run "v1" version1;
  test_run "v2" version2;
  test_run "v2" version2;
  test_run "v2" version2;
  test_run "v3" version3;
  test_run "v3" version3;
  test_run "v3" version3
;;

let main () =
  Random.init 42;
  let delta = 1. in
  print_endline "block_interval,alpha,k,cutoff,model,i,p";
  let row ~block_interval ~alpha ~k ~cutoff ~model ~i p =
    Printf.printf "%g,%g,%i,%i,%s,%i,%g\n%!" block_interval alpha k cutoff model i p
  in
  let job ~block_interval ~alpha ~cutoff =
    let lambda = 1. /. block_interval
    and tau = 200. *. block_interval in
    let versions ~k =
      [ ("v0", fun () -> run (version0 ~k ~cutoff ~lambda ~alpha ~delta ~tau))
      ; ("v0c", fun () -> run (version0_compliant ~k ~cutoff ~lambda ~alpha ~delta ~tau))
      ; ( "v0p"
        , fun () -> run (version0_pre ~atk_plus:0 ~k ~cutoff ~lambda ~alpha ~delta ~tau)
        )
        (* ; ("v1", fun () -> run (version1 ~k ~cutoff  ~lambda ~alpha ~delta ~tau)) *)
        (* ; ("v2", fun () -> run (version2 ~k  ~cutoff ~lambda ~alpha ~delta ~tau)) *)
        (* ; ("v3", fun () -> run (version3 ~k  ~cutoff ~lambda ~alpha ~delta ~tau)) *)
      ]
    in
    for k = 1 to 10 do
      let () =
        let open GR22AFT in
        let p = { k; delta; lambda; rho = 1. -. alpha } in
        row ~block_interval ~alpha ~k ~cutoff:0 ~model:"GR22AFT.T1.lower" ~i:0 (t1lower p);
        row ~block_interval ~alpha ~k ~cutoff:0 ~model:"GR22AFT.T1.upper" ~i:0 (t1upper p);
        row ~block_interval ~alpha ~k ~cutoff:0 ~model:"GR22AFT.T2.lower" ~i:0 (t2lower p);
        row ~block_interval ~alpha ~k ~cutoff:0 ~model:"GR22AFT.T2.upper" ~i:0 (t2upper p)
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
              row ~block_interval ~cutoff ~alpha ~k ~model ~i p)
          done)
        (versions ~k)
    done
  in
  List.iter
    (fun block_interval ->
      List.iter
        (fun alpha ->
          List.iter
            (fun cutoff -> job ~block_interval ~alpha ~cutoff)
            (* cutoff *) [ 0; 42 ])
        (* alphas *) [ 0.1; 0.25; 0.33 ])
    (* block intervals *) [ 4.; 8.; 16. ]
;;

let () =
  match Bos.OS.Env.req_var "TEST" with
  | Ok _ -> test ()
  | Error _ -> main ()
;;
