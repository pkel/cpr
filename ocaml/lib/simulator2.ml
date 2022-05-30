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
  | Activate of { node : int }
  | Deliver of
      { node : int
      ; msg : 'prot_data env Dag.vertex
      }

type 'prot_data clock =
  { mutable now : float
  ; mutable queue : (float, 'prot_data event) OrderedQueue.t
  ; mutable c_activations : int
  }

type ('prot_data, 'node_state) node' =
  { mutable state : 'node_state
  ; mutable n_activations : int
  ; activate : 'node_state -> 'node_state
  ; deliver : 'node_state -> 'prot_data env Dag.vertex -> 'node_state
  ; preferred : 'node_state -> 'prot_data env Dag.vertex
  }

type 'prot_data node = Node : ('prot_data, 'node_state) node' -> 'prot_data node

type 'prot_data state =
  { clock : 'prot_data clock
  ; dag : 'prot_data env Dag.t
  ; global_view : 'prot_data env Dag.view
  ; global_view_m : ('prot_data env, 'prot_data) Intf2.global_view
  ; nodes : 'prot_data node array
  ; assign_pow_distr : int Distributions.iid
  ; activation_delay_distr : float Distributions.iid
  ; network : Network.t
  ; judge : 'prot_data env Dag.vertex list -> 'prot_data env Dag.vertex
  }

let schedule time delay event =
  time.queue <- OrderedQueue.queue (time.now +. delay) event time.queue
;;

let schedule_activation state =
  let delay = Distributions.sample state.activation_delay_distr
  and node = Distributions.sample state.assign_pow_distr in
  schedule state.clock delay (Activate { node })
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
        schedule clock delay (Deliver { node = link.dest; msg = x })))
    network.nodes.(source).links
;;

let string_of_pow_hash (nonce, _serial) =
  Printf.sprintf "%.3f" (float_of_int nonce /. (2. ** 29.))
;;

let init
    (type a)
    (module Protocol : Intf2.Protocol with type data = a)
    ?(patch : (int -> (a env, a) Intf2.local_view -> (a env, a) Intf2.node) option)
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
  let impl =
    match patch with
    | Some f -> f
    | None -> fun _node_id -> Protocol.honest
  in
  let nodes =
    Array.init n_nodes (fun node_id ->
        (* TODO breakout and reuse for RL gyms *)
        let module LocalView = struct
          include GlobalView

          let my_id = node_id
          let visible x = Float.Array.get (Dag.data x).delivered_at node_id <= clock.now
          let view = Dag.filter visible GlobalView.view
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
        let share_single ?(recursive = false) =
          let rec f x =
            assert (LocalView.visible x);
            let d = Dag.data x in
            if d.released_at > clock.now
            then (
              d.released_at <- min d.released_at clock.now;
              disseminate network clock node_id x;
              if recursive then List.iter f (Dag.parents LocalView.view x))
          in
          f
        in
        let share (r : _ Intf2.handler_return) =
          List.iter share_single r.share;
          r.state
        in
        let activate state =
          let payload = Node.puzzle_payload state in
          let pow_hash = Some (Random.bits (), Dag.size dag) in
          let vertex =
            Dag.append
              dag
              payload.parents
              { value = payload.data
              ; received_at =
                  Float.Array.init n_nodes (fun i ->
                      if i = Node.my_id then clock.now else Float.infinity)
              ; delivered_at =
                  Float.Array.init n_nodes (fun i ->
                      if i = Node.my_id then clock.now else Float.infinity)
              ; appended_at = clock.now
              ; appended_by = Some Node.my_id
              ; pow_hash
              ; signed_by = (if payload.sign then Some Node.my_id else None)
              ; released_at = Float.infinity
              }
          in
          let () =
            (* We guarantee that invalid extensions are never delivered elsewhere *)
            if not (Protocol.dag_validity (module GlobalView) vertex)
            then (
              let info x =
                [ Protocol.describe x.value, ""
                ; ( "node"
                  , Option.map string_of_int x.appended_by |> Option.value ~default:"n/a"
                  )
                ; "time", Printf.sprintf "%.2f" x.appended_at
                ; ( "hash"
                  , Option.map string_of_pow_hash x.pow_hash
                    |> Option.value ~default:"n/a" )
                ]
              in
              Dag.Exn.raise GlobalView.view info [ vertex ] "invalid append")
          in
          Node.handler state (PuzzleSolved vertex) |> share
        in
        let deliver state msg = Node.handler state (Deliver msg) |> share in
        Node
          { state = Node.init ~roots
          ; n_activations = 0
          ; activate
          ; deliver
          ; preferred = Node.preferred
          })
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
    ; assign_pow_distr
    ; activation_delay_distr
    ; network
    ; judge = Protocol.judge (module GlobalView)
    }
  in
  schedule_activation state;
  state
;;

let handle_event ~activations state ev =
  match ev with
  | Activate x ->
    let (Node node) = state.nodes.(x.node) in
    state.clock.c_activations <- state.clock.c_activations + 1;
    node.n_activations <- node.n_activations + 1;
    (* check ending condition; schedule next activation *)
    if state.clock.c_activations < activations || activations < 0
    then schedule_activation state;
    (* apply event handler *)
    node.state <- node.activate node.state
  | Deliver x ->
    let was_delivered msg =
      Float.Array.get (Dag.data msg).delivered_at x.node <= state.clock.now
    and was_received msg =
      Float.Array.get (Dag.data msg).received_at x.node <= state.clock.now
    and disseminate =
      match state.network.dissemination with
      | Flooding -> disseminate state.network state.clock x.node
      | Simple -> fun _ -> ()
    in
    let (Node node) = state.nodes.(x.node) in
    (* deliver DAG vertex exactly once to each network node as soon as all parent DAG
       vertices have been delivered *)
    if was_delivered x.msg
    then (* n was delivered before *) ()
    else if List.exists
              (fun dedepency -> was_delivered dedepency |> not)
              (Dag.parents state.global_view x.msg)
    then (* dependencies are not yet fulfilled *) ()
    else (
      (* deliver; continue broadcast; recurse *)
      Float.Array.set (Dag.data x.msg).delivered_at x.node state.clock.now;
      node.state <- node.deliver node.state x.msg;
      disseminate x.msg;
      (* recursive delivery of now unlocked dependent DAG vertices *)
      List.iter
        (fun msg ->
          if was_received msg && not (was_delivered msg)
          then schedule state.clock 0. (Deliver { x with msg }))
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
    handle_event ~activations state ev;
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
