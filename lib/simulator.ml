(* proof of work -- invalidated on first use *)
type pow = { mutable fresh : bool }

let pow () = { fresh = true }

(* data attached to each DAG node *)
type 'a sim_data =
  { value : 'a
  ; visibility : bool array (* could be a bitfield *)
  }

type 'prot_data sim_event =
  { node : int
  ; event : ('prot_data sim_data, pow) Protocol.event
  }

type 'prot_data sim_time =
  { mutable now : float
  ; mutable queue : (float, 'prot_data sim_event) OrderedQueue.t
  ; mutable activations : int
  }

type ('prot_data, 'node_state) sim_state =
  { time : 'prot_data sim_time
  ; dag : 'prot_data sim_data Dag.t
  ; global_view : 'prot_data sim_data Dag.view
  ; node_state : 'node_state array
  ; node_ctx : ('prot_data sim_data, 'prot_data, pow) Protocol.context array
  ; protocol : ('prot_data sim_data, 'prot_data, 'node_state, pow) Protocol.protocol
  }

type sim_params =
  { n_nodes : int
  ; n_activations : int
  ; activation_delay : float
  ; message_delay : float
  }

let schedule time delay event =
  time.queue <- OrderedQueue.queue (time.now +. delay) event time.queue
;;

let draw_exponential ~ev = -1. *. ev *. log (Random.float 1.)

let schedule_activation params time =
  let delay = draw_exponential ~ev:params.activation_delay
  and node = Random.int params.n_nodes in
  schedule time delay { node; event = Activate (pow ()) }
;;

let init params protocol : _ sim_state =
  let open Protocol in
  let dag, roots =
    let visibility = Array.make params.n_nodes true in
    List.map (fun value -> { value; visibility }) protocol.dag_roots |> Dag.roots
  in
  let time = { queue = OrderedQueue.init Float.compare; now = 0.; activations = 0 } in
  let () = schedule_activation params time in
  let global_view = Dag.view dag in
  let node_state = Array.init params.n_nodes (fun _i -> protocol.init ~roots) in
  let node_ctx =
    Array.init params.n_nodes (fun i ->
        let data n = (Dag.data n).value in
        { view = Dag.filter (fun n -> n.visibility.(i)) global_view
        ; data
        ; release =
            (fun n ->
              for i' = 0 to params.n_nodes - 1 do
                if i' <> i
                then (
                  let delay = draw_exponential ~ev:params.message_delay in
                  schedule time delay { node = i'; event = Deliver n })
              done)
        ; extend_dag =
            (fun ?pow parents child ->
              let pow =
                (* check pow *)
                match pow with
                | Some ({ fresh = true } as x) ->
                  x.fresh <- false;
                  true
                | Some { fresh = false } -> raise Protocol.Invalid_dag_extension
                | None -> false
              in
              let () =
                (* check dag invariant *)
                let parents = List.map data parents in
                if not (protocol.dag_invariant ~pow ~parents ~child)
                then raise Invalid_dag_extension
              in
              let visibility = Array.init params.n_nodes (fun i' -> i' = i) in
              Dag.append dag parents { value = child; visibility })
        })
  in
  { time; dag; global_view; node_ctx; node_state; protocol }
;;

let handle_event params state ev =
  let () =
    match ev.event with
    | Activate _pow ->
      state.time.activations <- state.time.activations + 1;
      if state.time.activations < params.n_activations
      then schedule_activation params state.time
    | Deliver gnode -> (Dag.data gnode).visibility.(ev.node) <- true
  in
  state.node_state.(ev.node)
    <- state.protocol.event_handler
         state.node_ctx.(ev.node)
         state.node_state.(ev.node)
         ev.event
;;

let rec loop params state =
  match OrderedQueue.dequeue state.time.queue with
  | Some (now, ev, queue) ->
    state.time.now <- now;
    state.time.queue <- queue;
    handle_event params state ev;
    loop params state
  | None -> state
;;
