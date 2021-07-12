(* proof of work -- invalidated on first use *)
type pow = { mutable fresh : bool }

let pow () = { fresh = true }

(* data attached to each DAG node *)
type 'a data =
  { value : 'a
  ; delivered_at : floatarray
  ; appended_by : int option
  ; appended_at : float
  }

type 'prot_data event =
  { node : int
  ; event : ('prot_data data, pow) Protocol.event
  }

type 'prot_data clock =
  { mutable now : float
  ; mutable queue : (float, 'prot_data event) OrderedQueue.t
  ; mutable c_activations : int
  }

type 'prot_data node =
  | SNode :
      { mutable state : 'node_state
      ; mutable n_activations : int
      ; handler : 'node_state -> ('prot_data data, pow) Protocol.event -> 'node_state
      ; preferred : 'node_state -> 'prot_data data Dag.node
      }
      -> 'prot_data node

type ('prot_data, 'node_state) state =
  { clock : 'prot_data clock
  ; dag : 'prot_data data Dag.t
  ; global_view : 'prot_data data Dag.view
  ; nodes : 'prot_data node array
  ; assign_pow : int Distributions.iid
  }

type params =
  { network : Network.t
  ; activations : int
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

let disseminate params clock source x =
  List.iter
    (fun link ->
      let open Network in
      let delivered_at = (Dag.data x).delivered_at in
      let t = Float.Array.get delivered_at link.dest
      and delay = link.delay () in
      let t' = clock.now +. delay in
      if t' < t
      then (
        (* only schedule event if it yields faster delivery *)
        Float.Array.set delivered_at link.dest t';
        schedule clock delay { node = link.dest; event = Deliver x }))
    params.network.nodes.(source).links
;;

let init
    params
    ?(deviations = Array.make (Array.length params.network.nodes) None)
    protocol
    : _ state
  =
  let open Protocol in
  let n_nodes = Array.length params.network.nodes in
  let dag = Dag.create () in
  let roots =
    let delivered_at = Float.Array.make n_nodes 0. in
    List.map
      (fun value ->
        Dag.append dag [] { value; delivered_at; appended_by = None; appended_at = 0. })
      protocol.dag_roots
  in
  let clock = { queue = OrderedQueue.init Float.compare; now = 0.; c_activations = 0 }
  and global_view = Dag.view dag in
  let nodes =
    Array.init n_nodes (fun node ->
        let read d = d.value
        and view =
          Dag.filter
            (fun x -> Float.Array.get (Dag.data x).delivered_at node <= clock.now)
            global_view
        and share x = disseminate params clock node x
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
          let delivered_at =
            Float.Array.init n_nodes (fun i ->
                if i = node then clock.now else Float.infinity)
          in
          Dag.append
            dag
            parents
            { value = child
            ; delivered_at
            ; appended_at = clock.now
            ; appended_by = Some node
            }
        in
        let (Node participant) =
          match deviations.(node) with
          | None -> protocol.honest { view; read }
          | Some p -> p { view; read }
        in
        SNode
          { handler = participant.handler { share; extend_dag }
          ; state = participant.init ~roots
          ; preferred = participant.preferred
          ; n_activations = 0
          })
  and assign_pow =
    let weights =
      Array.map (fun x -> Network.(x.compute)) params.network.nodes |> Array.to_list
    in
    Distributions.discrete ~weights
  in
  let state = { clock; dag; global_view; nodes; assign_pow } in
  schedule_activation params state;
  state
;;

let handle_event params state ev =
  let (SNode node) = state.nodes.(ev.node) in
  let apply () = node.state <- node.handler node.state ev.event in
  match ev.event, params.network.dissemination with
  | Activate _pow, _ ->
    state.clock.c_activations <- state.clock.c_activations + 1;
    node.n_activations <- node.n_activations + 1;
    (* check ending condition; schedule next activation *)
    if state.clock.c_activations < params.activations
    then schedule_activation params state;
    (* apply event handler *)
    apply ()
  | Deliver _, Simple -> apply ()
  | Deliver gnode, Flooding ->
    (* deliver only once *)
    if state.clock.now >= Float.Array.get (Dag.data gnode).delivered_at ev.node
    then (
      (* continue broadcast *)
      disseminate params state.clock ev.node gnode;
      (* apply event handler *)
      apply ())
;;

let rec loop params state =
  match OrderedQueue.dequeue state.clock.queue with
  | Some (now, ev, queue) ->
    assert (now >= state.clock.now);
    state.clock.now <- now;
    state.clock.queue <- queue;
    handle_event params state ev;
    loop params state
  | None -> state
;;

let apply_reward_function (fn : _ Protocol.reward_function) head state =
  let arr = Array.make (Array.length state.nodes) 0. in
  let reward x n =
    match (Dag.data n).appended_by with
    | Some i -> arr.(i) <- arr.(i) +. x
    | None -> ()
  and read n = n.value in
  fn { Protocol.view = state.global_view; read } reward head;
  arr
;;
