(* data attached to each DAG vertex *)
type 'a env =
  { value : 'a (* protocol data *)
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
  | StochasticClock
  | Receive of
      { dst : int
      ; msg : 'prot_data env Dag.vertex
      }
  | ForNode of
      { node : int
      ; event : 'prot_data env Intf2.event
      }

type 'prot_data clock =
  { mutable now : float
  ; mutable queue : (float, 'prot_data event) OrderedQueue.t
  ; mutable c_activations : int
  }

type ('prot_data, 'node_state) node' =
  { mutable state : 'node_state
  ; visibility : 'prot_data env Dag.vertex -> bool
  ; handler :
      'node_state
      -> 'prot_data env Intf2.event
      -> ('prot_data env, 'node_state) Intf2.handler_return
  ; puzzle_payload : 'node_state -> ('prot_data env, 'prot_data) Intf2.puzzle_payload
  ; preferred : 'node_state -> 'prot_data env Dag.vertex
  }

type 'prot_data node = Node : ('prot_data, 'node_state) node' -> 'prot_data node

type 'prot_data state =
  { clock : 'prot_data clock
  ; dag : 'prot_data env Dag.t
  ; global_view : 'prot_data env Dag.view
  ; global_view_m : ('prot_data env, 'prot_data) Intf2.global_view
  ; nodes : 'prot_data node array
  ; activations : int array
  ; assign_pow_distr : int Distributions.iid
  ; activation_delay_distr : float Distributions.iid
  ; network : Network.t
  ; check_dag : 'prot_data env Dag.vertex -> unit
  ; judge : 'prot_data env Dag.vertex list -> 'prot_data env Dag.vertex
  ; mutable done_ : bool (* set to true for shutdown *)
  }

let schedule clock delay event =
  clock.queue <- OrderedQueue.queue (clock.now +. delay) event clock.queue
;;

let schedule_proof_of_work state =
  let delay = Distributions.sample state.activation_delay_distr in
  schedule state.clock delay StochasticClock
;;

let disseminate network clock source x =
  let open Network in
  List.iter
    (fun link ->
      let received_at = (Dag.data x).received_at in
      let t = Float.Array.get received_at link.dest
      and delay = Distributions.sample link.delay in
      let t' = clock.now +. delay in
      if t' < t
      then (
        (* only schedule event if it enables faster delivery *)
        Float.Array.set received_at link.dest t';
        schedule clock delay (Receive { dst = link.dest; msg = x })))
    network.nodes.(source).links
;;

let string_of_pow_hash (nonce, _serial) =
  Printf.sprintf "%.3f" (float_of_int nonce /. (2. ** 29.))
;;

let init
    (type a)
    (module Protocol : Intf2.Protocol with type data = a)
    ?(patch :
       (int -> ((a env, a) Intf2.local_view -> (a env, a) Intf2.node) option) option)
    (network : Network.t)
    : a state
  =
  let dag = Dag.create () in
  let n_nodes = Array.length network.nodes in
  let module GlobalView = struct
    type data = Protocol.data
    type nonrec env = data env

    let view = Dag.view dag
    let data n = (Dag.data n).value
    let signed_by n = (Dag.data n).signed_by
    let pow_hash n = (Dag.data n).pow_hash
  end
  in
  (* let module _ : Intf2.GlobalView = GlobalView in *)
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
      Protocol.dag_roots
  in
  let clock = { queue = OrderedQueue.init Float.compare; now = 0.; c_activations = 0 } in
  let patch = Option.value patch ~default:(fun _ -> None) in
  let impl node_id =
    match patch node_id with
    | Some f -> f
    | None -> Protocol.honest
  in
  let nodes =
    Array.init n_nodes (fun node_id ->
        let visibility x =
          Float.Array.get (Dag.data x).delivered_at node_id <= clock.now
        in
        let module LocalView = struct
          include GlobalView

          let my_id = node_id
          let view = Dag.filter visibility GlobalView.view
          let delivered_at n = Float.Array.get (Dag.data n).delivered_at node_id
          let appended_by_me n = (Dag.data n).appended_by = Some node_id

          let data x =
            (* assert (visible x); TODO investigate why this fails*)
            data x
          ;;

          let released n = (Dag.data n).released_at <= clock.now
        end
        in
        let (Node (module Node)) = impl node_id (module LocalView) in
        Node
          { state = Node.init ~roots
          ; visibility
          ; puzzle_payload = Node.puzzle_payload
          ; handler = Node.handler
          ; preferred = Node.preferred
          })
  and check_dag vertex =
    (* We guarantee that invalid extensions are never delivered elsewhere *)
    if not (Protocol.dag_validity (module GlobalView) vertex)
    then (
      let info x =
        [ Protocol.describe x.value, ""
        ; "node", Option.map string_of_int x.appended_by |> Option.value ~default:"n/a"
        ; "time", Printf.sprintf "%.2f" x.appended_at
        ; "hash", Option.map string_of_pow_hash x.pow_hash |> Option.value ~default:"n/a"
        ]
      in
      Dag.Exn.raise GlobalView.view info [ vertex ] "invalid append")
  and assign_pow_distr =
    let weights =
      Array.map (fun x -> Network.(x.compute)) network.nodes |> Array.to_list
    in
    Distributions.discrete ~weights
  and activation_delay_distr = Distributions.exponential ~ev:network.activation_delay in
  let state =
    { clock
    ; dag
    ; global_view = GlobalView.view
    ; global_view_m = (module GlobalView)
    ; nodes
    ; activations = Array.make 0 n_nodes
    ; assign_pow_distr
    ; activation_delay_distr
    ; network
    ; done_ = false
    ; check_dag
    ; judge = Protocol.judge (module GlobalView)
    }
  in
  schedule_proof_of_work state;
  state
;;

let mine state node_id =
  let (Node node) = state.nodes.(node_id) in
  let payload = node.puzzle_payload node.state in
  let pow_hash = Some (Random.bits (), Dag.size state.dag) in
  let n_nodes = Array.length state.nodes in
  Dag.append
    state.dag
    payload.parents
    { value = payload.data
    ; received_at =
        Float.Array.init n_nodes (fun i ->
            if i = node_id then state.clock.now else Float.infinity)
    ; delivered_at =
        Float.Array.init n_nodes (fun i ->
            if i = node_id then state.clock.now else Float.infinity)
    ; appended_at = state.clock.now
    ; appended_by = Some node_id
    ; pow_hash
    ; signed_by = (if payload.sign then Some node_id else None)
    ; released_at = Float.infinity
    }
;;

let handle_event state ev =
  match ev with
  | ForNode x ->
    let (Node node) = state.nodes.(x.node) in
    let ret : _ Intf2.handler_return = node.handler node.state x.event in
    let rec share msg =
      assert (node.visibility msg);
      let d = Dag.data msg in
      if d.released_at > state.clock.now
      then (
        d.released_at <- min d.released_at state.clock.now;
        disseminate state.network state.clock x.node msg;
        (* recursive *)
        List.iter share (Dag.parents (Dag.filter node.visibility state.global_view) msg))
    in
    List.iter share ret.share;
    node.state <- ret.state
  | StochasticClock ->
    if not state.done_
    then (
      let node_id = Distributions.sample state.assign_pow_distr in
      let vertex = mine state node_id in
      let () = state.check_dag vertex in
      schedule
        state.clock
        0.
        (ForNode { node = node_id; event = PuzzleSolved vertex });
      state.clock.c_activations <- state.clock.c_activations + 1;
      state.activations.(node_id) <- state.activations.(node_id) + 1;
      schedule_proof_of_work state)
  | Receive x ->
    (* might happen multiple times per message per node; internal to simulator: does not
       interact with node directly *)
    let was_delivered msg =
      Float.Array.get (Dag.data msg).delivered_at x.dst <= state.clock.now
    and was_received msg =
      Float.Array.get (Dag.data msg).received_at x.dst <= state.clock.now
    and disseminate =
      match state.network.dissemination with
      | Flooding -> disseminate state.network state.clock x.dst
      | Simple -> fun _ -> ()
    in
    (* deliver DAG vertex exactly once to each network node as soon as all parent DAG
       vertices have been delivered *)
    if was_delivered x.msg
    then (* x was delivered before, ignore *) ()
    else if List.exists
              (fun dep -> was_delivered dep |> not)
              (Dag.parents state.global_view x.msg)
    then (* dependencies are not yet fulfilled, wait *) ()
    else (
      (* deliver *)
      Float.Array.set (Dag.data x.msg).delivered_at x.dst state.clock.now;
      schedule state.clock 0. (ForNode { node = x.dst; event = Deliver x.msg });
      (* continue broadcast *)
      disseminate x.msg;
      (* reconsider now unlocked dependent DAG vertices recursively *)
      List.iter
        (fun msg ->
          if was_received msg && not (was_delivered msg)
          then schedule state.clock 0. (Receive { x with msg }))
        (Dag.children state.global_view x.msg))
;;

let dequeue state =
  OrderedQueue.dequeue state.clock.queue
  |> Option.map (fun (now, ev, queue) ->
         assert (now >= state.clock.now);
         state.clock.now <- now;
         state.clock.queue <- queue;
         ev)
;;

let rec loop ~activations state =
  match dequeue state with
  | None -> ()
  | Some ev ->
    handle_event state ev;
    if state.clock.c_activations >= activations then state.done_ <- true;
    loop ~activations state
;;

let apply_reward_function' (fn : _ Intf2.reward_function) seq state =
  let arr = Array.make (Array.length state.nodes) 0. in
  let assign x n =
    match (Dag.data n).appended_by with
    | Some i -> arr.(i) <- arr.(i) +. x
    | None -> ()
  and view = state.global_view_m in
  Seq.iter (fn ~view ~assign) seq;
  arr
;;

let apply_reward_function (fn : _ Intf2.reward_function) state =
  let head =
    Array.map
      (function
        | Node node -> node.preferred node.state)
      state.nodes
    |> Array.to_list
    |> state.judge
  in
  apply_reward_function' fn (Dag.iterate_ancestors state.global_view [ head ]) state
;;