(** Simulator events as they are applied to single network nodes *)
type 'env event =
  | PuzzleSolved of 'env Dag.vertex
  | Deliver of 'env Dag.vertex

module type GlobalView = sig
  (** what the simulator stores on each DAG vertex. Opaque to the protocol *)
  type env

  (** what the protocol stores on each DAG vertex *)
  type data

  (** (partial) view on the simulator's DAG *)
  val view : env Dag.view

  (** read the protocol data attached to the DAG vertex *)
  val data : env Dag.vertex -> data

  (** return the id of the signer, if the DAG vertex was signed. *)
  val signed_by : env Dag.vertex -> int option

  (** return the proof-of-work hash of the DAG vertex, if the vertex was attached with
      proof-of-work authorization. *)
  val pow_hash : env Dag.vertex -> (int * int) option
end

type ('a, 'b) global_view = (module GlobalView with type env = 'a and type data = 'b)

module type LocalView = sig
  include GlobalView

  val my_id : int

  (** when was the DAG vertex delivered locally? *)
  val delivered_at : env Dag.vertex -> float

  (** was the local vertex already shared with the network? *)
  val released : env Dag.vertex -> bool

  (** was the vertex appended locally (true) or by another node (false) *)
  val appended_by_me : env Dag.vertex -> bool

  (* TODO: some protocols extend the DAG without pow. Add extend_dag function here, to
     support this case *)
end

type ('a, 'b) local_view = (module LocalView with type env = 'a and type data = 'b)

(** decisions to be made by a node handler *)
type ('env, 'data, 'state) handler_return =
  { state : 'state (** new state *)
  ; share : 'env Dag.vertex list
        (** vertices to be shared with the other nodes. All vertices are shared
            recursively, i.e., including their parents *)
  }

(** what a node currently mines on *)
type ('env, 'data) puzzle_payload =
  { parents : 'env Dag.vertex list (** hash-references to previous DAG vertices *)
  ; data : 'data (** protocol data attached to the DAG vertices *)
  ; sign : bool (** whether to include a signature or not *)
  }

module type Node = sig
  include LocalView

  type state

  (** initialization. The [roots] argument holds references to global versions of
      {protocol.dag_roots}. The roots are visible to all nodes from the beginning. *)
  val init : roots:env Dag.vertex list -> state

  (** event handlers *)
  val handler : state -> env event -> (env, data, state) handler_return

  (** [puzzle_payload ~sign state] defines the content and parents of the currently mined
      vertex. When the node solves a proof-of-work puzzle, the simulator calls
      [puzzle_payload], constructs a corresponding DAG vertex, and hands it to the mining
      node. The simulator raises {Invalid_argument} if the proposed extension does not
      satisfy the DAG invariant specified by the simulated protocol. *)
  val puzzle_payload : state -> (env, data) puzzle_payload

  (** returns a node's preferred DAG vertex, typically a leave. Rewards are calculated
      backwards from here *)
  val preferred : state -> env Dag.vertex
end

(** we hide the node's state type from the simulation. *)
type ('a, 'b) node =
  | Node :
      (module Node with type env = 'a and type data = 'b and type state = 'c)
      -> ('a, 'b) node

(** Calculate and assign rewards to a vertex and (potentially) its neighbours. Use this
    together with {!Dag.iterate_ancestors}. *)
type ('a, 'b) reward_function =
  view:('a, 'b) global_view
  -> assign:(float -> 'a Dag.vertex -> unit)
  -> 'a Dag.vertex
  -> unit

module type Protocol = sig
  (** what the protocol stores on each DAG vertex *)
  type data

  (** a short identifier of the protocol. Used in filenames and as dictionary key. *)
  val key : string

  (** a concise description of the protocol *)
  val info : string

  (** used for pretty printing protocol DAG vertices. *)
  val describe : data -> string

  (** block height *)
  val height : data -> int

  (** specify the roots of the DAG. *)
  val dag_roots : data list

  (** restrict DAG extensions. The simulator checks validity for each appended DAG vertex.
      Invalid extensions are not delivered to other nodes. *)
  val dag_validity : ('env, data) global_view -> 'env Dag.vertex -> bool

  (** specification for honest participants *)
  val honest : ('env, data) local_view -> ('env, data) node

  (** When calculating rewards, the simulator will consider the preferred vertices of all
      nodes. This functions determines the best vertex. The reward function will be
      applied from the best vertex backwards to the roots. *)
  val judge : ('env, data) global_view -> 'env Dag.vertex list -> 'env Dag.vertex

  val reward_functions : ('env, data) reward_function Collection.t
  val attacks : (('env, data) local_view -> ('env, data) node) Collection.t
end
