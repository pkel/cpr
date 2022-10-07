(** Simulator events as they are applied to single network nodes. Something
  turns visible on the DAG. Either appended on request, received from the
    network, or authorized with proof-of-work. *)
type 'env event =
  | Append of 'env Dag.vertex
  | Network of 'env Dag.vertex
  | ProofOfWork of 'env Dag.vertex

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
  val signature : env Dag.vertex -> int option

  (** return the proof-of-work hash of the DAG vertex, if the vertex was attached with
      proof-of-work authorization. *)
  val pow : env Dag.vertex -> (int * int) option

  val max_pow : int * int
  val min_pow : int * int

  (* TODO. Hide type of hash. Provide comparison here. *)
end

type ('a, 'b) global_view = (module GlobalView with type env = 'a and type data = 'b)

(** Calculate and assign rewards for the vertex and (potentially) its neighbours.
    Typically this is called on the history of the winning chain. *)
type 'a reward_function = assign:(float -> 'a Dag.vertex -> unit) -> 'a Dag.vertex -> unit

(* TODO change reward function to something more coinbase-like: *)
(* type 'a reward_function = 'a Dag.vertex -> (int * float) list *)

module type Referee = sig
  include GlobalView

  (** restrict DAG extensions. The simulator checks validity for each appended DAG vertex.
      Invalid extensions are not delivered to other nodes. *)
  val dag_validity : env Dag.vertex -> bool

  (** When calculating rewards, the simulator will consider the preferred vertices of all
      nodes. This functions determines the best vertex. The reward function will be
      applied from the best vertex backwards to the roots. *)
  val winner : env Dag.vertex list -> env Dag.vertex

  (** Extract linear history of the given (tip of) chain. For Bitcoin, this returns the
      blockchain in reverse order. For Ethereum it returns the sequence of blocks w/o
      uncles in reverse order. *)
  val history : env Dag.vertex -> env Dag.vertex Seq.t

  val reward_functions : env reward_function Collection.t
end

type ('a, 'b) referee = (module Referee with type env = 'a and type data = 'b)

type ('env, 'data) draft_vertex =
  { parents : 'env Dag.vertex list (** hash-references to previous DAG vertices *)
  ; data : 'data (** protocol data attached to the DAG vertices *)
  ; sign : bool (** whether to include a signature or not *)
  }

module type LocalView = sig
  include GlobalView

  val my_id : int

  (* Who sees the vertex? Received implies that vertex was first received via network.
     Released implies that the vertex was appended locally and then shared. Withheld
     implies appended locally but not (yet) shared. *)
  val visibility : env Dag.vertex -> [ `Received | `Released | `Withheld ]

  (** Since when is the DAG vertex visible? *)
  val visible_since : env Dag.vertex -> float
end

type ('a, 'b) local_view = (module LocalView with type env = 'a and type data = 'b)

type ('env, 'data, 'state) action =
  { state : 'state (** future state *)
  ; share : 'env Dag.vertex list
        (** vertices to be shared with the other nodes. Withheld vertices are
            shared recursively, i.e., including their parents. *)
  ; append : ('env, 'data) draft_vertex list (** vertices to be appended to the DAG *)
  }

let return ?(share = []) ?(append = []) state = { state; share; append }

module type Node = sig
  include LocalView

  type state

  (** initialization. The [roots] argument holds references to global versions of
      {protocol.dag_roots}. The roots are visible to all nodes from the beginning. *)
  val init : roots:env Dag.vertex list -> state

  (** event handlers *)
  val handler : state -> env event -> (env, data, state) action

  (** [puzzle_payload state] defines the content and parents of the currently mined
      vertex. When the node solves a proof-of-work puzzle, the simulator calls
      [puzzle_payload], constructs a corresponding DAG vertex, and hands it to the mining
      node. The simulator raises {Invalid_argument} if the proposed extension does not
      satisfy the DAG invariant specified by the simulated protocol. *)
  val puzzle_payload : state -> (env, data) draft_vertex

  (** returns a node's preferred DAG vertex, typically a leave. Rewards are calculated
      backwards from here *)
  val preferred : state -> env Dag.vertex
end

(** we hide the node's state type from the simulation. *)
type ('a, 'b) node =
  | Node :
      (module Node with type env = 'a and type data = 'b and type state = 'c)
      -> ('a, 'b) node

module type Protocol = sig
  (** what the protocol stores on each DAG vertex *)
  type data

  (** a short identifier of the protocol. Used in filenames and as dictionary key. *)
  val key : string

  (** a concise description of the protocol *)
  val info : string

  (** some protocols accumulate multiple puzzle solutions into a single block. E.g., Bₖ
      has k puzzles per block. Nakamoto has one per block. *)
  val puzzles_per_block : int

  (** used for pretty printing protocol DAG vertices. *)
  val describe : data -> string

  (** block height. *)
  val height : data -> int

  (** blockchain progress. Measures work spent on the blockchain. Equals height for
      Nakamoto. Includes uncles for Ethereum. Counts votes in B_k.

      DAA tries to keep progress/time constant. *)
  val progress : data -> float

  (** specify the roots of the DAG. *)
  val dag_roots : data list

  (** specification of global truths *)
  val referee : ('env, data) global_view -> ('env, data) referee

  (** specification for honest participants *)
  val honest : ('env, data) local_view -> ('env, data) node
end

type protocol = Protocol : (module Protocol with type data = 'a) -> protocol

module type AttackSpace = sig
  val key : string
  val info : string

  module Protocol : Protocol

  module Observation : sig
    type t

    val length : int
    val low : t
    val high : t
    val to_floatarray : t -> floatarray
    val of_floatarray : floatarray -> t
    val to_string : t -> string
  end

  module Action : sig
    type t

    val n : int
    val to_string : t -> string
    val to_int : t -> int
    val of_int : int -> t
  end

  module Agent (V : LocalView with type data = Protocol.data) : sig
    open V

    type state
    type observable_state

    val preferred : state -> env Dag.vertex
    val puzzle_payload : state -> (env, data) draft_vertex
    val init : roots:env Dag.vertex list -> state
    val prepare : state -> env event -> observable_state
    val observe : observable_state -> Observation.t
    val apply : observable_state -> Action.t -> (env, data, state) action
  end

  val policies : (Observation.t -> Action.t) Collection.t

  val attacker
    :  (Observation.t -> Action.t)
    -> ('env, Protocol.data) local_view
    -> ('env, Protocol.data) node
end

type attack_space =
  | AttackSpace : (module AttackSpace with type Protocol.data = 'a) -> attack_space
