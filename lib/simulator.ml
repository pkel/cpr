(* proof of work -- invalidated on first use *)
type pow = { mutable fresh : bool }

let pow () = { fresh = true }

(* data attached to each DAG node *)
type 'a data =
  { value : 'a
  ; received_at : floatarray
  ; delivered_at : floatarray
        (* delivery happens when all dependencies in the DAG are fulfilled *)
  ; appended_by : int option
  ; appended_at : float
  ; mutable released_at : float
  ; pow_hash : (int * int) option
  ; signed_by : int option
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
  ; global : ('prot_data data, 'prot_data) Protocol.global_view
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
      let received_at = (Dag.data x).received_at in
      let t = Float.Array.get received_at link.dest
      and delay = link.delay () in
      let t' = clock.now +. delay in
      if t' < t
      then (
        (* only schedule event if it enables faster delivery *)
        Float.Array.set received_at link.dest t';
        schedule clock delay { node = link.dest; event = Deliver x }))
    params.network.nodes.(source).links
;;

let init
    params
    ?(deviations = Array.make (Array.length params.network.nodes) None)
    (protocol : _ Protocol.protocol)
    : _ state
  =
  let n_nodes = Array.length params.network.nodes in
  let dag = Dag.create () in
  let roots =
    let delivered_at = Float.Array.make n_nodes 0.
    and received_at = Float.Array.make n_nodes 0. in
    List.map
      (fun value ->
        Dag.append
          dag
          []
          { value
          ; delivered_at
          ; received_at
          ; appended_by = None
          ; appended_at = 0.
          ; released_at = 0.
          ; signed_by = None
          ; pow_hash = None
          })
      protocol.dag_roots
  in
  let clock = { queue = OrderedQueue.init Float.compare; now = 0.; c_activations = 0 }
  and global : _ Protocol.global_view =
    let data n = (Dag.data n).value
    and signed_by n = (Dag.data n).signed_by
    and pow_hash n = (Dag.data n).pow_hash in
    { view = Dag.view dag; data; signed_by; pow_hash }
  in
  let nodes =
    Array.init n_nodes (fun node ->
        let view =
          Dag.filter
            (fun x -> Float.Array.get (Dag.data x).delivered_at node <= clock.now)
            global.view
        and delivered_at n = Float.Array.get (Dag.data n).delivered_at node
        and appended_by_me n = (Dag.data n).appended_by = Some node
        and share n =
          let d = Dag.data n in
          d.released_at <- min d.released_at clock.now;
          disseminate params clock node n
        and released n = (Dag.data n).released_at <= clock.now
        and extend_dag ?pow ?(sign = false) parents child =
          let pow_hash =
            (* check pow *)
            match pow with
            | Some x when x.fresh ->
              x.fresh <- false;
              (* ensure uniqueness of pow hashes *)
              Some (Random.bits (), Dag.size dag)
            | Some _ -> raise (Invalid_argument "pow was used before")
            | None -> None
          in
          let node =
            Dag.append
              dag
              parents
              { value = child
              ; received_at =
                  Float.Array.init n_nodes (fun i ->
                      if i = node then clock.now else Float.infinity)
              ; delivered_at =
                  Float.Array.init n_nodes (fun i ->
                      if i = node then clock.now else Float.infinity)
              ; appended_at = clock.now
              ; appended_by = Some node
              ; pow_hash
              ; signed_by = (if sign then Some node else None)
              ; released_at = Float.infinity
              }
          in
          if not (protocol.dag_validity global node)
          then
            (* We assume that invalid extensions are never delivered elsewhere *)
            failwith "invalid DAG extension";
          node
        in
        let (Node participant) =
          let view : _ Protocol.local_view =
            { my_id = node
            ; view
            ; data = global.data
            ; signed_by = global.signed_by
            ; pow_hash = global.pow_hash
            ; delivered_at
            ; released
            ; appended_by_me
            }
          in
          match deviations.(node) with
          | None -> protocol.honest view
          | Some p -> p view
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
  let state = { clock; dag; global; nodes; assign_pow } in
  schedule_activation params state;
  state
;;

let handle_event params state ev =
  let (SNode node) = state.nodes.(ev.node) in
  let was_delivered n =
    Float.Array.get (Dag.data n).delivered_at ev.node <= state.clock.now
  and was_received n = Float.Array.get (Dag.data n).received_at ev.node <= state.clock.now
  and disseminate =
    match params.network.dissemination with
    | Flooding -> disseminate params state.clock ev.node
    | Simple -> fun _n -> ()
  in
  match ev.event with
  | Activate _pow ->
    state.clock.c_activations <- state.clock.c_activations + 1;
    node.n_activations <- node.n_activations + 1;
    (* check ending condition; schedule next activation *)
    if state.clock.c_activations < params.activations
    then schedule_activation params state;
    (* apply event handler *)
    node.state <- node.handler node.state ev.event
  | Deliver n ->
    (* deliver dag node exactly once to each network node as soon as all parent dag nodes
       have been delivered *)
    if was_delivered n
    then (* n was delivered before *) ()
    else if List.exists
              (fun n -> was_delivered n |> not)
              (Dag.parents state.global.view n)
    then (* dependencies are not yet fulfilled *) ()
    else (
      (* deliver; continue broadcast; recurse *)
      Float.Array.set (Dag.data n).delivered_at ev.node state.clock.now;
      node.state <- node.handler node.state ev.event;
      disseminate n;
      (* recursive delivery of now unlocked dependent DAG nodes *)
      List.iter
        (fun n ->
          if was_received n && not (was_delivered n)
          then schedule state.clock 0. { node = ev.node; event = Deliver n })
        (Dag.children state.global.view n))
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
  let assign x n =
    match (Dag.data n).appended_by with
    | Some i -> arr.(i) <- arr.(i) +. x
    | None -> ()
  and view = state.global in
  Seq.iter (fn ~view ~assign) (Dag.iterate_ancestors view.view [ head ]);
  arr
;;
