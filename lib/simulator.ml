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
  ; assign_pow : int Distributions.iid
  }

type params =
  { network : Network.t
  ; n_activations : int
  ; activation_delay : float
  }

let schedule time delay event =
  time.queue <- OrderedQueue.queue (time.now +. delay) event time.queue
;;

let schedule_activation params state =
  let delay = Distributions.exponential ~ev:params.activation_delay ()
  and node = state.assign_pow () in
  schedule state.clock delay { node; event = Activate (pow ()) }
;;

let propagate params clock node x =
  (* TODO: speed up gossip broadcast simulation *)
  (* We introduced a major slowdown here. We can speed up by queuing less. [visibility:
     bool array] could be replaced by a "deliver_at: floatarray". Then enqueue only
     earlier deliveries and update deliver_at. *)
  List.iter
    (fun link ->
      let open Network in
      if not (Dag.data x).visibility.(link.dest)
      then (
        let delay = link.delay () in
        schedule clock delay { node = link.dest; event = Deliver x }))
    params.network.(node).links
;;

let init params protocol : _ state =
  let open Protocol in
  let n_nodes = Array.length params.network in
  let dag, roots =
    let visibility = Array.make n_nodes true in
    List.map (fun value -> { value; visibility }) protocol.dag_roots |> Dag.roots
  in
  let clock = { queue = OrderedQueue.init Float.compare; now = 0.; activations = 0 }
  and global_view = Dag.view dag in
  let nodes =
    Array.init n_nodes (fun node ->
        let read d = d.value
        and view = Dag.filter (fun x -> x.visibility.(node)) global_view
        and share x = propagate params clock node x
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
          let visibility = Array.init n_nodes (fun i -> i = node) in
          Dag.append dag parents { value = child; visibility }
        in
        let implementation = protocol.spawn { view; read; share; extend_dag } in
        { handler = implementation.handler; state = implementation.init ~roots })
  and assign_pow =
    let weights =
      Array.map (fun x -> Network.(x.compute)) params.network |> Array.to_list
    in
    Distributions.discrete ~weights
  in
  let state = { clock; dag; global_view; nodes; assign_pow } in
  schedule_activation params state;
  state
;;

let handle_event params state ev =
  let apply () =
    let node = state.nodes.(ev.node) in
    node.state <- node.handler node.state ev.event
  in
  match ev.event with
  | Activate _pow ->
    state.clock.activations <- state.clock.activations + 1;
    (* check ending condition; schedule next activation *)
    if state.clock.activations < params.n_activations
    then schedule_activation params state;
    (* apply event handler *)
    apply ()
  | Deliver gnode ->
    let visibility = (Dag.data gnode).visibility in
    (* deliver only once *)
    if not visibility.(ev.node)
    then (
      visibility.(ev.node) <- true;
      (* continue broadcast *)
      propagate params state.clock ev.node gnode;
      (* apply event handler *)
      apply ())
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
