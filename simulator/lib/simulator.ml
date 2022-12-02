(* data attached to each block in the DAG *)
type 'a env =
  { value : 'a (* protocol data *)
  ; pow : (int * int) option
  ; signature : int option
  ; visibility :
      [ `Invisible | `Visible of float * [ `Received | `Released | `Withheld ] ] array
  ; received_at : floatarray (* received but potentially not yet visible *)
  ; rewards : floatarray (* accumulated rewards up to this vertex *)
  }

type 'a block = 'a env Dag.vertex

let timestamp x =
  Array.map
    (function
      | `Invisible -> Float.infinity
      | `Visible (x, _) -> x)
    x.visibility
  |> Array.fold_left Float.min Float.infinity
;;

(* TODO. These feel a bit superfluous. But yesterday's attempt to remove did not succeed.
   Take a look at the RL engine before the next try. *)
type 'a from_node_event =
  | Share of 'a block
  | Append of ('a block, 'a) Intf.block_draft
  | PowProposal of ('a block, 'a) Intf.block_draft

type 'a event =
  | StochasticClock
  | Dag of int * [ `Append | `ProofOfWork ] * ('a block, 'a) Intf.block_draft
  | Network of int * [ `Rx | `Tx ] * 'a block
  | OnNode of int * 'a block Intf.event
  | MakeVisible of int * [ `Append | `Network | `ProofOfWork ] * 'a block
  | MadeVisible of int * [ `Append | `Network | `ProofOfWork ] * 'a block

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
  | Dag (node, kind, _draft) ->
    let kind =
      match kind with
      | `Append -> "Append"
      | `ProofOfWork -> "ProofOfWork"
    in
    sprintf "Network (node %i, %s, draft _)" node kind
  | Network (node, kind, msg) ->
    let kind =
      match kind with
      | `Rx -> "Rx"
      | `Tx -> "Tx"
    in
    sprintf "Network (node %i, %s, msg %s)" node kind (string_of_vertex view msg)
  | MakeVisible (node, kind, vertex) ->
    let kind =
      match kind with
      | `Network -> "Network"
      | `Append -> "Append"
      | `ProofOfWork -> "ProofOfWork"
    in
    sprintf "MakeVisible (node %i, %s, vtx %s)" node kind (string_of_vertex view vertex)
  | MadeVisible (node, kind, vertex) ->
    let kind =
      match kind with
      | `Network -> "Network"
      | `Append -> "Append"
      | `ProofOfWork -> "ProofOfWork"
    in
    sprintf "MadeVisible (node %i, %s, vtx %s)" node kind (string_of_vertex view vertex)
  | OnNode (node, ev) ->
    let kind, vertex =
      match ev with
      | Append x -> "Append", x
      | Network x -> "Network", x
      | ProofOfWork x -> "ProofOfWork", x
    in
    sprintf "OnNode (node %i, %s, vtx %s)" node kind (string_of_vertex view vertex)
;;

type 'a clock =
  { mutable now : float
  ; mutable queue : (float, 'a event) OrderedQueue.t
  ; mutable c_activations : int
  }

type ('a, 'b) node' =
  { mutable state : 'b
  ; view : ('a block, 'a) Intf.view
  ; handler : 'b -> 'a block Intf.event -> ('a block, 'a, 'b) Intf.action
  ; puzzle_payload : 'b -> ('a block, 'a) Intf.block_draft
  ; preferred : 'b -> 'a block
  }

type 'a node = Node : ('a, 'b) node' -> 'a node

type 'a state =
  { clock : 'a clock
  ; dag : 'a env Dag.t
  ; roots : 'a block list
  ; global_view : 'a env Dag.view
  ; referee : ('a block, 'a) Intf.referee
  ; nodes : 'a node array
  ; activations : int array
  ; assign_pow_distr : int Distributions.iid
  ; activation_delay_distr : float Distributions.iid
  ; network : Network.t
  ; logger : Log.logger
  }

let float_of_pow_hash (nonce, _serial) = float_of_int nonce /. (2. ** 29.)
let string_of_pow_hash x = Printf.sprintf "%.3f" (float_of_pow_hash x)

let raw_append ~pow ~n_nodes dag node_id (x : _ Intf.block_draft) =
  let pow = if pow then Some (Random.bits (), Dag.size dag) else None
  and signature = if x.sign then Some node_id else None
  and visibility = Array.make n_nodes `Invisible in
  Dag.append
    dag
    x.parents
    { value = x.data
    ; pow
    ; signature
    ; visibility
    ; received_at = Float.Array.make n_nodes Float.infinity
    ; rewards = Float.Array.make n_nodes Float.nan
    }
;;

(** deterministic appends; required for Tailstorm. *)
let append ~pow ~n_nodes dag node_id (x : _ Intf.block_draft) =
  let default () = `Fresh, raw_append ~pow ~n_nodes dag node_id x in
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
    | Some x -> `Redundant, x
    | _ -> default ())
;;

let schedule clock delay event =
  let delay =
    match delay with
    | `Now -> 0.
    | `Delay x -> x
  in
  clock.queue <- OrderedQueue.queue (clock.now +. delay) event clock.queue
;;

let schedule_proof_of_work state =
  let x = Distributions.sample state.activation_delay_distr in
  schedule state.clock (`Delay x) StochasticClock
;;

let log_vertex log clock view info v =
  let open Info in
  let d = Dag.data v in
  let info =
    Option.map (fun x -> float "pow" (float_of_pow_hash x)) d.pow
    :: (info v |> List.map Option.some)
    |> List.filter_map Fun.id
  in
  log
    clock.now
    (Log.Vertex
       { id = Dag.id v
       ; parents = Dag.parents view v |> List.map Dag.id
       ; signature = d.signature
       ; info
       })
;;

let blockdag (type a) (view : a env Dag.view) : (a block, a) Intf.blockdag =
  (module struct
    type block = a env Dag.vertex
    type data = a

    let children = Dag.children view
    let parents = Dag.parents view

    module Block = struct
      type t = block

      let children = children
      let parents = parents
      let compare = Dag.compare_vertex
      let eq = Dag.vertex_eq
      let neq = Dag.vertex_neq
    end

    let data n = (Dag.data n).value
    let signature n = (Dag.data n).signature

    type pow = int * int

    let compare_pow = compare
    let pow n = (Dag.data n).pow
    let max_pow = max_int, max_int
    let min_pow = min_int, 0

    let raise_invalid_dag info blocks message =
      let info x = List.map (fun (k, v) -> k, Info.string_of_value v) (info x) in
      Dag.Exn.raise view info blocks ("invalid_dag: " ^ message)
    ;;
  end : Intf.BlockDAG
    with type data = a
     and type block = a block)
;;

let init
    (type a)
    ?(logger = Log.dummy_logger)
    (module Protocol : Intf.Protocol with type data = a)
    ?(patch : (int -> ((a block, a) Intf.view -> (a block, a) Intf.node) option) option)
    (network : Network.t)
    : a state
  =
  let dag = Dag.create ()
  and n_nodes = Array.length network.nodes in
  let global_view = Dag.view dag in
  let (module Ref) = Protocol.referee (blockdag global_view) in
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
          ; rewards = Float.Array.make n_nodes 0.
          })
      Protocol.roots
  in
  let clock =
    { queue = OrderedQueue.init Compare.(float); now = 0.; c_activations = 0 }
  in
  let () = List.iter (log_vertex logger clock global_view Ref.info) roots in
  let patch = Option.value patch ~default:(fun _ -> None) in
  let impl node_id =
    match patch node_id with
    | Some f -> f
    | None -> Protocol.honest
  in
  let nodes =
    Array.init n_nodes (fun node_id ->
        let visible x =
          match (Dag.data x).visibility.(node_id) with
          | `Visible _ -> true
          | `Invisible -> false
        in
        let view = Dag.filter visible global_view in
        let (module BlockDag) = blockdag view in
        let module View = struct
          include Ref
          include BlockDag

          let my_id = node_id

          let visibility x =
            match (Dag.data x).visibility.(node_id) with
            | `Visible (_, x) -> x
            | _ -> failwith "invalid access in simulator"
          ;;

          let visible_since x =
            match (Dag.data x).visibility.(node_id) with
            | `Visible (x, _) -> x
            | _ -> failwith "invalid access in simulator"
          ;;

          let data x =
            if visible x then data x else failwith "invalid access in simulator"
          ;;
        end
        in
        let (Node (module Node)) = impl node_id (module View) in
        Node
          { state = Node.init ~roots
          ; view = (module View)
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
    ; global_view
    ; referee = (module Ref)
    ; nodes
    ; activations = Array.make n_nodes 0
    ; assign_pow_distr
    ; activation_delay_distr
    ; network
    ; logger
    }
  in
  schedule_proof_of_work state;
  state
;;

let debug_info ?(info = fun _ -> []) x =
  let d = Dag.data x in
  let vis i = function
    | `Visible (x, `Received) -> Some (Printf.sprintf "n%i received at %.2f" i x, "")
    | `Visible (x, `Withheld) ->
      Some (Printf.sprintf "n%i appended at %.2f, withheld" i x, "")
    | `Visible (x, `Released) ->
      Some (Printf.sprintf "n%i appended at %.2f, released" i x, "")
    | `Invisible -> None
  in
  List.map (fun (k, v) -> Some (k, Info.string_of_value v)) (info x)
  @ [ Option.map (fun x -> "pow", string_of_pow_hash x) d.pow
    ; Option.map (fun x -> "signed by " ^ string_of_int x, "") d.signature
    ]
  @ (Array.mapi vis d.visibility |> Array.to_list)
  |> List.filter_map Fun.id
;;

(* We guarantee that invalid extensions are never delivered elsewhere *)
let check_vertex (type a) state vertex =
  let ((module Ref) : (a block, a) Intf.referee) = state.referee in
  if not (Ref.validity vertex)
  then
    Dag.Exn.raise
      state.global_view
      (debug_info ~info:Ref.info)
      (vertex :: Dag.parents state.global_view vertex)
      "invalid append"
;;

let log_vertex (type a) state vertex =
  let ((module Ref) : (a block, a) Intf.referee) = state.referee in
  log_vertex state.logger state.clock state.global_view Ref.info vertex
;;

let log_for_node state node_id =
  let open Log in
  function
  | Intf.Append x | ProofOfWork x ->
    state.logger state.clock.now (Event (node_id, Appends (Dag.id x)))
  | Network x -> state.logger state.clock.now (Event (node_id, Learns (Dag.id x)))
;;

let set_rewards (type a) state vertex =
  let ((module Ref) : (a block, a) Intf.referee) = state.referee in
  match Ref.precursor vertex with
  | None -> failwith "Referee.precursor should go back to DAG root."
  | Some p ->
    let open Float.Array in
    let r = (Dag.data vertex).rewards in
    (* copy values from precursor *)
    iteri (fun i x -> set r i x) (Dag.data p).rewards;
    (* add rewards for successor *)
    Ref.reward vertex |> List.iter (fun (i, x) -> set r i (get r i +. x))
;;

let append (type a) state ~pow node_id draft =
  let ((module Ref) : (a block, a) Intf.referee) = state.referee in
  match append ~n_nodes:(Array.length state.nodes) state.dag ~pow node_id draft with
  | `Redundant, x -> x
  | `Fresh, x ->
    check_vertex state x;
    set_rewards state x;
    log_vertex state x;
    x
;;

let handle_action state node_id (act : _ Intf.action) =
  let schedule = schedule state.clock in
  (* Recursive sharing of vertices. *)
  let rec share msg =
    let d = Dag.data msg in
    match d.visibility.(node_id) with
    | `Invisible -> failwith "invalid share"
    | `Visible (_, `Received) -> () (* not my business *)
    | `Visible (_, `Released) -> () (* shared before *)
    | `Visible (x, `Withheld) ->
      schedule `Now (Network (node_id, `Tx, msg));
      d.visibility.(node_id) <- `Visible (x, `Released);
      List.iter share (Dag.parents state.global_view msg)
  in
  List.iter share act.share;
  (* Append vertices. *)
  let append draft = schedule `Now (Dag (node_id, `Append, draft)) in
  List.iter append act.append
;;

let handle_event state ev =
  let schedule = schedule state.clock in
  match ev with
  | MakeVisible (node_id, kind, vtx) ->
    let visibility vtx = (Dag.data vtx).visibility.(node_id) in
    let visible vtx =
      match visibility vtx with
      | `Visible _ -> true
      | `Invisible -> false
    in
    if (not (visible vtx)) && List.for_all visible (Dag.parents state.global_view vtx)
    then (
      let arr = (Dag.data vtx).visibility in
      (* set visibility *)
      let visibility =
        match kind with
        | `Append | `ProofOfWork -> `Withheld
        | `Network -> `Received
      in
      arr.(node_id) <- `Visible (state.clock.now, visibility);
      (* inform node about new vertex *)
      let node_event =
        match kind with
        | `Append -> Intf.Append vtx
        | `Network -> Intf.Network vtx
        | `ProofOfWork -> Intf.ProofOfWork vtx
      in
      schedule `Now (OnNode (node_id, node_event));
      (* enable post-MakeVisible hooks *)
      schedule `Now (MadeVisible (node_id, kind, vtx)))
  | OnNode (node_id, node_event) ->
    assert (
      match node_event with
      | Network x | ProofOfWork x | Append x ->
        List.exists
          (fun x -> (Dag.data x).visibility.(node_id) = `Invisible)
          (x :: Dag.parents state.global_view x)
        |> not);
    let (Node node) = state.nodes.(node_id) in
    let act : _ Intf.action = node.handler node.state node_event in
    log_for_node state node_id node_event;
    (* Update state *)
    node.state <- act.state;
    handle_action state node_id act
  | StochasticClock ->
    let node_id = Distributions.sample state.assign_pow_distr in
    let (Node node) = state.nodes.(node_id) in
    let draft = node.puzzle_payload node.state in
    schedule `Now (Dag (node_id, `ProofOfWork, draft));
    state.clock.c_activations <- state.clock.c_activations + 1;
    state.activations.(node_id) <- state.activations.(node_id) + 1;
    schedule_proof_of_work state
  | Dag (node_id, kind, draft) ->
    let pow, kind =
      match kind with
      | `ProofOfWork -> true, `ProofOfWork
      | `Append -> false, `Append
    in
    let vertex = append ~pow state node_id draft in
    schedule `Now (MakeVisible (node_id, kind, vertex))
  | Network (src, `Tx, msg) ->
    List.iter
      (fun link ->
        let open Network in
        let delay = Distributions.sample link.delay in
        schedule (`Delay delay) (Network (link.dest, `Rx, msg)))
      state.network.nodes.(src).links
  | Network (node_id, `Rx, msg) ->
    let d = Dag.data msg in
    if state.clock.now < Float.Array.get d.received_at node_id
    then (
      Float.Array.set d.received_at node_id state.clock.now;
      schedule `Now (MakeVisible (node_id, `Network, msg)))
  | MadeVisible (node_id, _kind, vertex) ->
    let received_at msg = Float.Array.get (Dag.data msg).received_at node_id in
    (* continue flooding broadcast now that dependencies are delivered *)
    if state.network.dissemination = Flooding
    then
      if received_at vertex <= state.clock.now
      then schedule `Now (Network (node_id, `Tx, vertex))
      else ();
    (* reconsider of now unlocked vertices recursively *)
    let reconsider vtx =
      if received_at vtx <= state.clock.now
      then schedule `Now (MakeVisible (node_id, `Network, vtx))
    in
    List.iter reconsider (Dag.children state.global_view vertex)
;;

let dequeue state =
  OrderedQueue.dequeue state.clock.queue
  |> Option.map (fun (now, ev, queue) ->
         assert (now >= state.clock.now);
         state.clock.now <- now;
         state.clock.queue <- queue;
         ev)
;;

let loop ~activations state =
  let handle_event = handle_event state in
  let rec continue activations_left =
    match dequeue state with
    | None -> ()
    | Some StochasticClock when activations_left <= 0 -> continue activations_left
    | Some StochasticClock ->
      handle_event StochasticClock;
      continue (activations_left - 1)
    | Some ev ->
      handle_event ev;
      continue activations_left
  in
  continue activations
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

let history (type a) (state : a state) =
  let (module Ref) = state.referee in
  head state
  |> Seq.unfold (fun this -> Ref.precursor this |> Option.map (fun next -> this, next))
;;

let iter_coinbases (type a) (state : a state) =
  let (module Ref) = state.referee in
  head state
  |> Seq.unfold (fun this -> Ref.precursor this |> Option.map (fun next -> this, next))
  |> Seq.flat_map (fun x -> Ref.reward x |> List.to_seq)
;;
