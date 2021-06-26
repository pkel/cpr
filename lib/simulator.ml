(* proof of work -- invalidated on first use *)
type pow = { mutable fresh : bool }

let pow () = { fresh = true }

(* data attached to each DAG node *)
type 'a data =
  { value : 'a
  ; visibility : bool array (* could be a bitfield *)
  }

type 'prot_data event =
  { node : int
  ; event : ('prot_data data, pow) Protocol.event
  }

type 'prot_data clock =
  { mutable now : float
  ; mutable queue : (float, 'prot_data event) OrderedQueue.t
  ; mutable activations : int
  }

type ('prot_data, 'node_state) node =
  { mutable state : 'node_state
  ; handler : 'node_state -> ('prot_data data, pow) Protocol.event -> 'node_state
  }

type ('prot_data, 'node_state) state =
  { clock : 'prot_data clock
  ; dag : 'prot_data data Dag.t
  ; global_view : 'prot_data data Dag.view
  ; nodes : ('prot_data, 'node_state) node array
  }

type params =
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

let init params protocol : _ state =
  let open Protocol in
  let dag, roots =
    let visibility = Array.make params.n_nodes true in
    List.map (fun value -> { value; visibility }) protocol.dag_roots |> Dag.roots
  in
  let clock =
    let c = { queue = OrderedQueue.init Float.compare; now = 0.; activations = 0 } in
    schedule_activation params c;
    c
  and global_view = Dag.view dag in
  let nodes =
    Array.init params.n_nodes (fun node ->
        let read d = d.value
        and view = Dag.filter (fun x -> x.visibility.(node)) global_view
        and share x =
          for i = 0 to params.n_nodes - 1 do
            if i <> node
            then (
              let delay = draw_exponential ~ev:params.message_delay in
              schedule clock delay { node = i; event = Deliver x })
          done
        and extend_dag ?pow parents child =
          let pow =
            (* check pow *)
            match pow with
            | Some ({ fresh = true } as x) ->
              x.fresh <- false;
              true
            | Some { fresh = false } -> raise (Invalid_argument "pow was used before")
            | None -> false
          in
          let () =
            (* check dag invariant *)
            let parents = List.map (fun x -> (Dag.data x).value) parents in
            if not (protocol.dag_invariant ~pow parents child)
            then raise (Invalid_argument "dag invariant violated")
          in
          let visibility = Array.init params.n_nodes (fun i -> i = node) in
          Dag.append dag parents { value = child; visibility }
        in
        let implementation = protocol.spawn { view; read; share; extend_dag } in
        { handler = implementation.handler; state = implementation.init ~roots })
  in
  { clock; dag; global_view; nodes }
;;

let handle_event params state ev =
  let () =
    match ev.event with
    | Activate _pow ->
      state.clock.activations <- state.clock.activations + 1;
      if state.clock.activations < params.n_activations
      then schedule_activation params state.clock
    | Deliver gnode -> (Dag.data gnode).visibility.(ev.node) <- true
  in
  let node = state.nodes.(ev.node) in
  node.state <- node.handler node.state ev.event
;;

let rec loop params state =
  match OrderedQueue.dequeue state.clock.queue with
  | Some (now, ev, queue) ->
    state.clock.now <- now;
    state.clock.queue <- queue;
    handle_event params state ev;
    loop params state
  | None -> state
;;
