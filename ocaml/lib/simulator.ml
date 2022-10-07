(* data attached to each DAG vertex *)
type 'a env =
  { value : 'a (* protocol data *)
  ; pow : (int * int) option
  ; signature : int option
  ; visibility :
      [ `Invisible | `Visible of float * [ `Received | `Released | `Withheld ] ] array
  ; received_at : floatarray (* received but potentially not yet visible *)
  }

let timestamp x =
  Array.map
    (function
      | `Invisible -> Float.infinity
      | `Visible (x, _) -> x)
    x.visibility
  |> Array.fold_left Float.min Float.infinity
;;

type 'a network_event =
  | Rx of int * 'a env Dag.vertex
  | Tx of int * 'a env Dag.vertex

(* TODO. These feel a bit superfluous. But yesterday's attempt to remove did not succeed.
   Take a look at the RL engine before the next try. *)
type 'a from_node_event =
  | Share of 'a env Dag.vertex
  | Append of ('a env, 'a) Intf.draft_vertex
  | PowProposal of ('a env, 'a) Intf.draft_vertex

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
  | ForNode (node, Append x) ->
    sprintf "ForNode (node %i, Append %s)" node (string_of_vertex view x)
  | ForNode (node, ProofOfWork x) ->
    sprintf "ForNode (node %i, ProofOfWork %s)" node (string_of_vertex view x)
  | ForNode (node, Network x) ->
    sprintf "ForNode (node %i, Network %s)" node (string_of_vertex view x)
  | FromNode (node, Append _) -> sprintf "FromNode (node %i, Append _)" node
  | FromNode (node, PowProposal _) -> sprintf "FromNode (node %i, PuzzleProposal _)" node
  | FromNode (node, Share x) ->
    sprintf "FromNode (node %i, Share %s)" node (string_of_vertex view x)
;;

type 'prot_data clock =
  { mutable now : float
  ; mutable queue : (float * int, 'prot_data event) OrderedQueue.t
  ; mutable c_activations : int
  }

type ('prot_data, 'node_state) node' =
  { mutable state : 'node_state
  ; view : ('prot_data env, 'prot_data) Intf.local_view
  ; handler :
      'node_state
      -> 'prot_data env Intf.event
      -> ('prot_data env, 'prot_data, 'node_state) Intf.action
  ; puzzle_payload : 'node_state -> ('prot_data env, 'prot_data) Intf.draft_vertex
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

let string_of_pow_hash (nonce, _serial) =
  Printf.sprintf "%.3f" (float_of_int nonce /. (2. ** 29.))
;;

let debug_info ?describe x =
  let x = Dag.data x in
  let vis i = function
    | `Visible (x, `Received) -> Some (Printf.sprintf "n%i received at %.2f" i x, "")
    | `Visible (x, `Withheld) ->
      Some (Printf.sprintf "n%i appended at %.2f, withheld" i x, "")
    | `Visible (x, `Released) ->
      Some (Printf.sprintf "n%i appended at %.2f, released" i x, "")
    | `Invisible -> None
  in
  [ Option.map (fun f -> f x.value, "") describe
  ; Option.map (fun x -> "pow", string_of_pow_hash x) x.pow
  ; Option.map (fun x -> "signed by " ^ string_of_int x, "") x.signature
  ]
  @ (Array.mapi vis x.visibility |> Array.to_list)
  |> List.filter_map Fun.id
;;

let raw_append ~pow ~n_nodes clock dag node_id (x : _ Intf.draft_vertex) =
  let pow = if pow then Some (Random.bits (), Dag.size dag) else None
  and signature = if x.sign then Some node_id else None
  and visibility =
    let a = Array.make n_nodes `Invisible in
    a.(node_id) <- `Visible (clock.now, `Withheld);
    a
  in
  Dag.append
    dag
    x.parents
    { value = x.data
    ; pow
    ; signature
    ; visibility
    ; received_at = Float.Array.make n_nodes Float.infinity
    }
;;

(** deterministic appends; required for Tailstorm. *)
let append ~pow ~n_nodes clock dag node_id (x : _ Intf.draft_vertex) =
  let default () = `Global_fresh (raw_append ~pow ~n_nodes clock dag node_id x) in
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
      && dy.signature = None
      && List.for_all2 (fun a b -> Dag.id a = Dag.id b) (Dag.parents view y) x.parents
    in
    match List.find_opt eq candidates with
    | Some x ->
      let dx = Dag.data x in
      (match dx.visibility.(node_id) with
      | `Invisible ->
        dx.visibility.(node_id) <- `Visible (clock.now, `Withheld);
        `Local_fresh x
      | _ -> `Redundant x)
    | _ -> default ())
;;

let schedule clock delay event =
  let delay, prio =
    match delay with
    | `Immediate -> 0., 0
    | `Now -> 0., 1
    | `Delay x -> x, 1
  in
  clock.queue <- OrderedQueue.queue (clock.now +. delay, prio) event clock.queue
;;

let schedule_proof_of_work state =
  let x = Distributions.sample state.activation_delay_distr in
  schedule state.clock (`Delay x) StochasticClock
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
        let signature n = (Dag.data n).signature
        let pow n = (Dag.data n).pow
        let max_pow = max_int, max_int
        let min_pow = min_int, 0
      end)
  in
  (* let module _ : Intf.GlobalView = GlobalView in *)
  let roots =
    List.map
      (fun value ->
        Dag.append
          dag
          []
          { value
          ; signature = None
          ; pow = None
          ; received_at = Float.Array.make n_nodes 0.
          ; visibility = Array.make n_nodes (`Visible (0., `Received))
          })
      Protocol.dag_roots
  in
  let clock =
    { queue = OrderedQueue.init Compare.(tuple float int); now = 0.; c_activations = 0 }
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

          let visible x =
            match (Dag.data x).visibility.(node_id) with
            | `Visible _ -> true
            | `Invisible -> false
          ;;

          let view = Dag.filter visible Ref.view

          let visibility x =
            match (Dag.data x).visibility.(node_id) with
            | `Visible (_, x) -> x
            | _ -> failwith "invalid access"
          ;;

          let visible_since x =
            match (Dag.data x).visibility.(node_id) with
            | `Visible (x, _) -> x
            | _ -> failwith "invalid access"
          ;;

          let data x =
            assert (visible x);
            data x
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

let append state = append ~n_nodes:(Array.length state.nodes) state.clock state.dag

let handle_event state ev =
  let schedule = schedule state.clock in
  match ev with
  | ForNode (id, ev) ->
    let (Node node) = state.nodes.(id) in
    let act : _ Intf.action = node.handler node.state ev in
    node.state <- act.state;
    (* Share vertices. It's important to schedule this immediately. From now on, the agent
       might rely on the vertices being marked as released. This marking happens in the
       FromNode handler. *)
    List.iter (fun m -> schedule `Immediate (FromNode (id, Share m))) act.share;
    (* Append vertices. *)
    List.iter (fun m -> schedule `Now (FromNode (id, Append m))) act.append
  | StochasticClock ->
    if not state.done_
    then (
      let node_id = Distributions.sample state.assign_pow_distr in
      let (Node node) = state.nodes.(node_id) in
      let draft = node.puzzle_payload node.state in
      schedule `Now (FromNode (node_id, PowProposal draft));
      state.clock.c_activations <- state.clock.c_activations + 1;
      state.activations.(node_id) <- state.activations.(node_id) + 1;
      schedule_proof_of_work state)
  | FromNode (node_id, PowProposal draft) ->
    let vertex =
      match append ~pow:true state node_id draft with
      | `Global_fresh x -> x
      | _ -> assert false
    in
    let () = state.check_dag vertex in
    schedule `Now (ForNode (node_id, ProofOfWork vertex))
  | FromNode (node_id, Append draft) ->
    (match append ~pow:false state node_id draft with
    | `Redundant _ -> ()
    | `Global_fresh x | `Local_fresh x ->
      let () = state.check_dag x in
      schedule `Now (ForNode (node_id, Append x)))
  | FromNode (src, Share msg) ->
    (* recursive sharing *)
    let rec share msg =
      let d = Dag.data msg in
      match d.visibility.(src) with
      | `Invisible -> failwith "invalid share"
      | `Visible (x, `Withheld) ->
        schedule `Now (Network (Tx (src, msg)));
        d.visibility.(src) <- `Visible (x, `Released);
        (* recursive sharing of dependent vertices *)
        List.iter share (Dag.parents state.global_view msg)
      | `Visible (_, `Released) -> () (* shared before *)
      | `Visible (_, `Received) -> () (* shared by someone else *)
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
    (* TODO. We might have a problem here. Vertices turning visible through deterministic
       appends w/o being delivered might break the delivery logic. *)
    let rec h msg =
      (* deliver DAG vertex exactly once to each network node as soon as all parent DAG
         vertices have been delivered *)
      match (Dag.data msg).visibility.(dst) with
      | `Visible _ -> ()
      | `Invisible ->
        if List.exists
             (fun dep ->
               match (Dag.data dep).visibility.(dst) with
               | `Invisible -> true
               | _ -> false)
             (Dag.parents state.global_view msg)
        then (* dependencies are not yet fulfilled, wait *) ()
        else
          ((* deliver *)
           assert (received_at msg <= state.clock.now);
           (Dag.data msg).visibility.(dst) <- `Visible (state.clock.now, `Received);
           schedule `Now (ForNode (dst, Network msg));
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
  |> Option.map (fun ((now, _prio), ev, queue) ->
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
  (Dag.data vertex).visibility
  |> Array.to_list
  |> List.mapi (fun i x -> i, x)
  |> List.filter (function
         | _i, `Visible (_, `Released) -> true
         | _i, `Visible (_, `Withheld) -> true
         | _ -> false)
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
