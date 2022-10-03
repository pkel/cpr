(* data attached to each DAG vertex *)
type 'a env =
  { value : 'a (* protocol data *)
  ; received_at : floatarray
  ; delivered_at : floatarray
        (* delivery happens when all dependencies in the DAG are fulfilled *)
  ; appended_at : floatarray
  ; released_at : floatarray
  ; pow_hash : (int * int) option
  ; signed_by : int option
  }

type 'a network_event =
  | Rx of int * 'a env Dag.vertex
  | Tx of int * 'a env Dag.vertex

type 'a from_node_event =
  | Share of 'a env Dag.vertex
  | PuzzleProposal of ('a env, 'a) Intf.vertex_proposal

type 'prot_data event =
  | StochasticClock
  | Network of 'prot_data network_event
  | ForNode of int * 'prot_data env Intf.event
  | FromNode of int * 'prot_data from_node_event

let string_of_vertex view x =
  let open Printf in
  let id x = Dag.id x |> string_of_int in
  let parents = List.map id (Dag.parents view x) |> String.concat "|" in
  sprintf "%i[%s]" (Dag.id x) parents
;;

let string_of_event view =
  let open Printf in
  function
  | StochasticClock -> "StochasticClock"
  | Network (Rx (dst, msg)) ->
    sprintf "Network/Rx (dst %i, msg %s)" dst (string_of_vertex view msg)
  | Network (Tx (src, msg)) ->
    sprintf "Network/Tx (src %i, msg %s)" src (string_of_vertex view msg)
  | ForNode (node, PuzzleSolved x) ->
    sprintf "ForNode (node %i, PuzzleSolved %s)" node (string_of_vertex view x)
  | ForNode (node, Deliver x) ->
    sprintf "ForNode (node %i, Deliver %s)" node (string_of_vertex view x)
  | FromNode (node, Share x) ->
    sprintf "FromNode (node %i, Share %s)" node (string_of_vertex view x)
  | FromNode (node, PuzzleProposal _) ->
    sprintf "FromNode (node %i, PuzzleProposal _)" node
;;

type 'prot_data clock =
  { mutable now : float
  ; mutable queue : (int * float, 'prot_data event) OrderedQueue.t
  ; mutable c_activations : int
  }

type ('prot_data, 'node_state) node' =
  { mutable state : 'node_state
  ; view : ('prot_data env, 'prot_data) Intf.local_view
  ; handler :
      'node_state
      -> 'prot_data env Intf.event
      -> ('prot_data env, 'node_state) Intf.handler_return
  ; puzzle_payload : 'node_state -> ('prot_data env, 'prot_data) Intf.vertex_proposal
  ; preferred : 'node_state -> 'prot_data env Dag.vertex
  }

type 'prot_data node = Node : ('prot_data, 'node_state) node' -> 'prot_data node

type 'prot_data state =
  { clock : 'prot_data clock
  ; dag : 'prot_data env Dag.t
  ; roots : 'prot_data env Dag.vertex list
  ; global_view : 'prot_data env Dag.view
  ; referee : ('prot_data env, 'prot_data) Intf.referee
  ; nodes : 'prot_data node array
  ; activations : int array
  ; assign_pow_distr : int Distributions.iid
  ; activation_delay_distr : float Distributions.iid
  ; network : Network.t
  ; check_dag : 'prot_data env Dag.vertex -> unit
  ; mutable done_ : bool (* set to true for shutdown *)
  }

let schedule clock delay event =
  let prio, delay =
    match delay with
    | `Immediate -> 0, 0.
    | `Now -> 1, 0.
    | `Delay x -> 1, x
  in
  clock.queue <- OrderedQueue.queue (prio, clock.now +. delay) event clock.queue
;;

let schedule_proof_of_work state =
  let x = Distributions.sample state.activation_delay_distr in
  schedule state.clock (`Delay x) StochasticClock
;;

let string_of_pow_hash (nonce, _serial) =
  Printf.sprintf "%.3f" (float_of_int nonce /. (2. ** 29.))
;;

let extend_dag ~pow ~n_nodes clock dag node_id (x : _ Intf.vertex_proposal) =
  let pow_hash = if pow then Some (Random.bits (), Dag.size dag) else None in
  Dag.append
    dag
    x.parents
    { value = x.data
    ; appended_at =
        Float.Array.init n_nodes (fun i ->
            if i = node_id then clock.now else Float.infinity)
    ; released_at = Float.Array.make n_nodes Float.infinity
    ; received_at = Float.Array.make n_nodes Float.infinity
    ; delivered_at = Float.Array.make n_nodes Float.infinity
    ; pow_hash
    ; signed_by = (if x.sign then Some node_id else None)
    }
;;

(** deterministic appends; required for Tailstorm. *)
let extend_dag ~pow ~n_nodes clock dag node_id (x : _ Intf.vertex_proposal) =
  let default () = extend_dag ~pow ~n_nodes clock dag node_id x in
  if pow || x.sign (* assuming non-deterministic signatures here *)
  then default ()
  else (
    let view = Dag.view dag in
    let candidates =
      match x.parents with
      | [] -> Dag.roots dag
      | hd :: _tl -> Dag.children view hd (* siblings *)
    in
    let eq y =
      let dy = Dag.data y in
      dy.value = x.data
      && dy.signed_by = None
      && List.for_all2 (fun a b -> Dag.id a = Dag.id b) (Dag.parents view y) x.parents
    in
    match List.find_opt eq candidates with
    | Some x ->
      let dx = Dag.data x in
      let set_now arr =
        Float.Array.set arr node_id (min clock.now (Float.Array.get arr node_id))
      in
      set_now dx.appended_at;
      x
    | _ -> default ())
;;

let debug_info ?describe x =
  let x = Dag.data x in
  let array a =
    Float.Array.fold_left (fun s x -> s ^ "|" ^ Printf.sprintf "%.2f" x) "[|" a ^ "]"
  in
  let protocol_info =
    match describe with
    | None -> []
    | Some f -> [ f x.value, "" ]
  in
  protocol_info
  @ [ "appended", array x.appended_at
    ; "released", array x.released_at
    ; "received", array x.received_at
    ; "delivered", array x.delivered_at
    ; "hash", Option.map string_of_pow_hash x.pow_hash |> Option.value ~default:"n/a"
    ; "signed", Option.map string_of_int x.signed_by |> Option.value ~default:"n/a"
    ]
;;

let visibility clock node_id x =
  Float.Array.get (Dag.data x).delivered_at node_id <= clock.now
  || Float.Array.get (Dag.data x).appended_at node_id <= clock.now
;;

let init
    (type a)
    (module Protocol : Intf.Protocol with type data = a)
    ?(patch : (int -> ((a env, a) Intf.local_view -> (a env, a) Intf.node) option) option)
    (network : Network.t)
    : a state
  =
  let dag = Dag.create () in
  let n_nodes = Array.length network.nodes in
  let (module Ref) =
    Protocol.referee
      (module struct
        type data = Protocol.data
        type nonrec env = data env

        let view = Dag.view dag
        let data n = (Dag.data n).value
        let signed_by n = (Dag.data n).signed_by
        let pow_hash n = (Dag.data n).pow_hash
        let max_pow_hash = max_int, max_int
        let min_pow_hash = min_int, 0
      end)
  in
  (* let module _ : Intf.GlobalView = GlobalView in *)
  let roots =
    let array = Float.Array.make n_nodes in
    List.map
      (fun value ->
        Dag.append
          dag
          []
          { value
          ; delivered_at = array 0.
          ; received_at = array 0.
          ; appended_at = array Float.infinity
          ; released_at = array Float.infinity
          ; signed_by = None
          ; pow_hash = None
          })
      Protocol.dag_roots
  in
  let clock =
    { queue = OrderedQueue.init Compare.(tuple int float); now = 0.; c_activations = 0 }
  in
  let patch = Option.value patch ~default:(fun _ -> None) in
  let impl node_id =
    match patch node_id with
    | Some f -> f
    | None -> Protocol.honest
  in
  let check_dag vertex =
    (* We guarantee that invalid extensions are never delivered elsewhere *)
    if not (Ref.dag_validity vertex)
    then
      Dag.Exn.raise
        Ref.view
        (debug_info ~describe:Protocol.describe)
        (vertex :: Dag.parents Ref.view vertex)
        "invalid append"
  in
  let nodes =
    Array.init n_nodes (fun node_id ->
        let module LocalView = struct
          include Ref

          let my_id = node_id
          let view = Dag.filter (visibility clock node_id) Ref.view
          let delivered_at x = Float.Array.get (Dag.data x).delivered_at node_id
          let delivered x = delivered_at x <= clock.now

          let appended_by_me x =
            Float.Array.get (Dag.data x).appended_at node_id <= clock.now
          ;;

          let data x =
            assert (visibility clock node_id x);
            data x
          ;;

          let released x = Float.Array.get (Dag.data x).released_at node_id <= clock.now

          let extend_dag x =
            let v = extend_dag ~pow:false ~n_nodes clock dag node_id x in
            check_dag v;
            v
          ;;
        end
        in
        let (Node (module Node)) = impl node_id (module LocalView) in
        Node
          { state = Node.init ~roots
          ; view = (module LocalView)
          ; puzzle_payload = Node.puzzle_payload
          ; handler = Node.handler
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
    ; roots
    ; dag
    ; global_view = Ref.view
    ; referee = (module Ref)
    ; nodes
    ; activations = Array.make n_nodes 0
    ; assign_pow_distr
    ; activation_delay_distr
    ; network
    ; done_ = false
    ; check_dag
    }
  in
  schedule_proof_of_work state;
  state
;;

let extend_dag state =
  extend_dag ~n_nodes:(Array.length state.nodes) state.clock state.dag
;;

let handle_event state ev =
  let schedule = schedule state.clock in
  match ev with
  | ForNode (id, ev) ->
    let (Node node) = state.nodes.(id) in
    let ret : _ Intf.handler_return = node.handler node.state ev in
    (* It's important to schedule this immediately. From now on, the agent might rely on
       the vertices being marked as released. This marking happens in the FromNode
       handler. *)
    List.iter (fun m -> schedule `Immediate (FromNode (id, Share m))) ret.share;
    node.state <- ret.state
  | StochasticClock ->
    if not state.done_
    then (
      let node_id = Distributions.sample state.assign_pow_distr in
      let (Node node) = state.nodes.(node_id) in
      let payload = node.puzzle_payload node.state in
      schedule `Now (FromNode (node_id, PuzzleProposal payload));
      state.clock.c_activations <- state.clock.c_activations + 1;
      state.activations.(node_id) <- state.activations.(node_id) + 1;
      schedule_proof_of_work state)
  | FromNode (node_id, PuzzleProposal payload) ->
    let vertex = extend_dag ~pow:true state node_id payload in
    let () = state.check_dag vertex in
    schedule `Now (ForNode (node_id, PuzzleSolved vertex))
  | FromNode (src, Share msg) ->
    (* implements recursive sharing *)
    let rec share msg =
      assert (visibility state.clock src msg);
      let d = Dag.data msg in
      let released_at = Float.Array.get d.released_at src
      and appended_at = Float.Array.get d.appended_at src in
      if appended_at < Float.infinity && released_at > state.clock.now
      then (
        (* appended by src but not yet released *)
        assert (appended_at <= state.clock.now);
        Float.Array.set d.released_at src state.clock.now;
        schedule `Now (Network (Tx (src, msg)));
        (* recursive sharing of dependent vertices *)
        List.iter share (Dag.parents state.global_view msg))
    in
    share msg
  | Network (Tx (src, msg)) ->
    List.iter
      (fun link ->
        let open Network in
        let received_at = (Dag.data msg).received_at in
        let t = Float.Array.get received_at link.dest
        and delay = Distributions.sample link.delay in
        let t' = state.clock.now +. delay in
        if t' < t
        then (
          (* only schedule new event if it enables faster delivery *)
          Float.Array.set received_at link.dest t';
          schedule (`Delay delay) (Network (Rx (link.dest, msg)))))
      state.network.nodes.(src).links
  | Network (Rx (dst, msg)) ->
    let received_at msg = Float.Array.get (Dag.data msg).received_at dst in
    (* implements in-order delivery of vertices and flooding *)
    let rec h msg =
      (* deliver DAG vertex exactly once to each network node as soon as all parent DAG
         vertices have been delivered *)
      if visibility state.clock dst msg
      then (* msg was delivered before, ignore *) ()
      else if List.exists
                (fun dep -> not (visibility state.clock dst dep))
                (Dag.parents state.global_view msg)
      then (* dependencies are not yet fulfilled, wait *) ()
      else
        ((* deliver *)
         Float.Array.set (Dag.data msg).delivered_at dst state.clock.now;
         schedule `Now (ForNode (dst, Deliver msg));
         (* continue broadcast *)
         let () =
           match state.network.dissemination with
           | Flooding -> schedule `Now (Network (Tx (dst, msg)))
           | Simple -> ()
         in
         (* reconsider now unlocked dependent DAG vertices recursively *)
         List.iter (fun msg -> if received_at msg <= state.clock.now then h msg))
          (Dag.children state.global_view msg)
    in
    h msg
;;

let dequeue state =
  OrderedQueue.dequeue state.clock.queue
  |> Option.map (fun ((_prio, now), ev, queue) ->
         assert (now >= state.clock.now);
         state.clock.now <- now;
         state.clock.queue <- queue;
         ev)
;;

let logger state event =
  let open Printf in
  printf "%07.2f: %s\n" state.clock.now (string_of_event state.global_view event)
;;

let rec loop ~activations state =
  match dequeue state with
  | None -> ()
  | Some ev ->
    (* ignore (logger state ev); *)
    handle_event state ev;
    if state.clock.c_activations >= activations then state.done_ <- true;
    loop ~activations state
;;

let reward_recipient vertex =
  (Dag.data vertex).appended_at
  |> Float.Array.to_list
  |> List.mapi (fun i x -> i, x)
  |> List.filter (fun (_i, x) -> x < Float.infinity)
  |> function
  | [] -> None
  | [ (i, _) ] -> Some i
  | _ -> failwith "cannot assign reward to vertex appended by multiple nodes"
;;

let apply_reward_function' (fn : _ Intf.reward_function) seq state =
  let arr = Array.make (Array.length state.nodes) 0. in
  let assign x vertex =
    reward_recipient vertex |> Option.iter (fun i -> arr.(i) <- arr.(i) +. x)
  in
  Seq.iter (fn ~assign) seq;
  arr
;;

let head (type a) (state : a state) =
  let (module Ref) = state.referee in
  Array.map
    (function
      | Node node -> node.preferred node.state)
    state.nodes
  |> Array.to_list
  |> Ref.winner
;;

let apply_reward_function ~history (fn : _ Intf.reward_function) state =
  apply_reward_function' fn (history (head state)) state
;;
