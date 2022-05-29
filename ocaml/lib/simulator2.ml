(* proof of work -- invalidated on first use *)
type pow = { mutable fresh : bool }

let pow () = { fresh = true }

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
  { node : int
  ; event : ('prot_data env, pow) Intf.event
  }

type 'prot_data clock =
  { mutable now : float
  ; mutable queue : (float, 'prot_data event) OrderedQueue.t
  ; mutable c_activations : int
  }

type ('prot_data, 'node_state) node' =
  { mutable state : 'node_state
  ; mutable n_activations : int
  ; handler : 'node_state -> ('prot_data env, pow) Intf.event -> 'node_state
  ; preferred : 'node_state -> 'prot_data env Dag.vertex
  }

type 'prot_data node = Node : ('prot_data, 'node_state) node' -> 'prot_data node

type 'prot_data state =
  { clock : 'prot_data clock
  ; dag : 'prot_data env Dag.t
  ; global_view : 'prot_data env Dag.view
  ; global_compat : ('prot_data env, 'prot_data) Intf.global_view
        (* TODO remove. used for reward function *)
  ; nodes : 'prot_data node array
  ; assign_pow_distr : int Distributions.iid
  ; activation_delay_distr : float Distributions.iid
  ; network : Network.t
  }

type wrapped_protocol =
  | Protocol : ('dag_data env, 'dag_data, pow, 'state) Intf.protocol -> wrapped_protocol

let schedule time delay event =
  time.queue <- OrderedQueue.queue (time.now +. delay) event time.queue
;;

let schedule_activation state =
  let delay = Distributions.sample state.activation_delay_distr
  and node = Distributions.sample state.assign_pow_distr in
  schedule state.clock delay { node; event = Activate (pow ()) }
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
        schedule clock delay { node = link.dest; event = Deliver x }))
    network.nodes.(source).links
;;

let spawn (n : _ Intf.node') ~roots actions =
  { handler = n.handler actions
  ; state = n.init ~roots
  ; preferred = n.preferred
  ; n_activations = 0
  }
;;

let string_of_pow_hash (nonce, _serial) =
  Printf.sprintf "%.3f" (float_of_int nonce /. (2. ** 29.))
;;

let all_honest
    (type a)
    (module Protocol : Intf2.Protocol with type data = a)
    (network : Network.t)
    : _ state * _ Dag.vertex list * (_ Intf2.local_view * _ Intf.actions) array
  =
  let dag = Dag.create () in
  let n_nodes = Array.length network.nodes in
  let module Env = struct
    type data = Protocol.data
    type nonrec env = data env
    type nonrec pow = pow
  end
  in
  let module Protocol' = Protocol.Make (Env) in
  let module GlobalView = struct
    include Env

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
      Protocol'.dag_roots
  in
  let clock = { queue = OrderedQueue.init Float.compare; now = 0.; c_activations = 0 } in
  let views_actions =
    Array.init n_nodes (fun node_id ->
        (* TODO breakout and reuse for RL gyms *)
        let module LocalView = struct
          include GlobalView

          let visible x = Float.Array.get (Dag.data x).delivered_at node_id <= clock.now
          let view = Dag.filter visible GlobalView.view
          let delivered_at n = Float.Array.get (Dag.data n).delivered_at node_id
          let appended_by_me n = (Dag.data n).appended_by = Some node_id

          let data x =
            (* assert (visible x); TODO investigate why this fails*)
            data x
          ;;

          let share ?(recursive = false) =
            let rec f x =
              assert (visible x);
              let d = Dag.data x in
              if d.released_at > clock.now
              then (
                d.released_at <- min d.released_at clock.now;
                disseminate network clock node_id x;
                if recursive then List.iter f (Dag.parents view x))
            in
            f
          ;;

          let released n = (Dag.data n).released_at <= clock.now

          let extend_dag ?pow ?(sign = false) parents child =
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
            let vertex =
              Dag.append
                dag
                parents
                { value = child
                ; received_at =
                    Float.Array.init n_nodes (fun i ->
                        if i = node_id then clock.now else Float.infinity)
                ; delivered_at =
                    Float.Array.init n_nodes (fun i ->
                        if i = node_id then clock.now else Float.infinity)
                ; appended_at = clock.now
                ; appended_by = Some node_id
                ; pow_hash
                ; signed_by = (if sign then Some node_id else None)
                ; released_at = Float.infinity
                }
            in
            if not (Protocol'.dag_validity (module GlobalView) vertex)
            then (
              (* We guarantee that invalid extensions are never delivered elsewhere *)
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
              Dag.Exn.raise GlobalView.view info [ vertex ] "invalid append");
            vertex
          ;;

          let my_id = node_id
        end
        in
        (* let module _ : Intf2.LocalView = LocalView in *)
        let local_view : (Env.env, Protocol.data, pow) Intf2.local_view =
          (module LocalView)
        in
        let actions : _ Intf.actions =
          { share = LocalView.share; extend_dag = LocalView.extend_dag }
        in
        local_view, actions)
  and assign_pow_distr =
    let weights =
      Array.map (fun x -> Network.(x.compute)) network.nodes |> Array.to_list
    in
    Distributions.discrete ~weights
  and activation_delay_distr = Distributions.exponential ~ev:network.activation_delay in
  let nodes =
    Array.map
      (fun (view, actions) ->
        let (Node (module Honest)) = Protocol'.honest view in
        Node
          { handler = Honest.handler actions
          ; state = Honest.init ~roots
          ; preferred = Honest.preferred
          ; n_activations = 0
          })
      views_actions
  in
  let state =
    { clock
    ; dag
    ; global_view = GlobalView.view
    ; global_compat =
        { view = GlobalView.view
        ; data = GlobalView.data
        ; signed_by = GlobalView.signed_by
        ; pow_hash = GlobalView.pow_hash
        }
    ; nodes
    ; assign_pow_distr
    ; activation_delay_distr
    ; network
    }
  in
  schedule_activation state;
  state, roots, views_actions
;;

let patch ~node impl (state, roots, views_actions) =
  let (view : _ Intf.local_view), (actions : _ Intf.actions) = views_actions.(node) in
  let n = spawn (impl view) ~roots actions in
  state.nodes.(node) <- Node n;
  view, actions, n
;;

(* TODO hide second and third element *)
let init (state, _roots, _views_actions) = state

let handle_event ~activations state ev =
  let (Node node) = state.nodes.(ev.node) in
  let was_delivered n =
    Float.Array.get (Dag.data n).delivered_at ev.node <= state.clock.now
  and was_received n = Float.Array.get (Dag.data n).received_at ev.node <= state.clock.now
  and disseminate =
    match state.network.dissemination with
    | Flooding -> disseminate state.network state.clock ev.node
    | Simple -> fun _n -> ()
  in
  match ev.event with
  | Activate _pow ->
    state.clock.c_activations <- state.clock.c_activations + 1;
    node.n_activations <- node.n_activations + 1;
    (* check ending condition; schedule next activation *)
    if state.clock.c_activations < activations || activations < 0
    then schedule_activation state;
    (* apply event handler *)
    node.state <- node.handler node.state ev.event
  | Deliver n ->
    (* deliver DAG vertex exactly once to each network node as soon as all parent DAG
       vertices have been delivered *)
    if was_delivered n
    then (* n was delivered before *) ()
    else if List.exists
              (fun n -> was_delivered n |> not)
              (Dag.parents state.global_view n)
    then (* dependencies are not yet fulfilled *) ()
    else (
      (* deliver; continue broadcast; recurse *)
      Float.Array.set (Dag.data n).delivered_at ev.node state.clock.now;
      node.state <- node.handler node.state ev.event;
      disseminate n;
      (* recursive delivery of now unlocked dependent DAG vertices *)
      List.iter
        (fun n ->
          if was_received n && not (was_delivered n)
          then schedule state.clock 0. { node = ev.node; event = Deliver n })
        (Dag.children state.global_view n))
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

let apply_reward_function' (fn : _ Intf.reward_function) seq state =
  let arr = Array.make (Array.length state.nodes) 0. in
  let assign x n =
    match (Dag.data n).appended_by with
    | Some i -> arr.(i) <- arr.(i) +. x
    | None -> ()
  and view = state.global_compat in
  Seq.iter (fn ~view ~assign) seq;
  arr
;;

let apply_reward_function (fn : _ Intf.reward_function) head state =
  apply_reward_function' fn (Dag.iterate_ancestors state.global_view [ head ]) state
;;
