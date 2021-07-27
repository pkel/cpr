open Cpr_lib
open Cpr_protocols
open Intf

(* TODO: This approach will not work. See todo note below. I think I'll have to

   1. split/rewrite Simulator.handle_event to make it work on a single, not encapsulated
   Simulator.(SNode node)

   2. maintain attacker state w/o hiding its type in SNode

   3. adapt Simulator.step for the new use case *)

let test : _ env =
  let actions = Array.of_list B_k.actions in
  let params : Simulator.params = assert false in
  let k = 51 in
  let init () =
    let deviations = Array.make (Array.length params.network.nodes) None in
    let observation = ref None in
    deviations.(0)
      <- Some
           B_k.(
             strategic ~k (fun v st ->
                 observation := Some (v, st);
                 noop_tactic v st));
    let state = Simulator.init ~deviations params (B_k.protocol ~k) in
    state, observation
  in
  let rec loop state =
    match Simulator.step params state with
    | Some ev -> if ev.node = 0 then () else loop state
    | None -> failwith "simulation should continue forever"
  in
  let create () =
    let state, args = init () in
    loop state;
    ref state, args
  in
  let reset (state, obs) =
    let state', obs' = init () in
    state := state';
    obs := !obs';
    loop !state;
    Option.get !obs
  in
  let step (state, obs) ~action:i =
    let v, st = Option.get !obs in
    let pa = assert false in
    let st' = B_k.apply_action ~k v pa st actions.(i) in
    ignore st';
    ignore state;
    (* TODO I cannot feed back st' into state.nodes.(0) *)
    assert false
  in
  Env { n_actions = Array.length actions; create; reset; step }
;;
