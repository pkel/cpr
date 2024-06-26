module type BlockDAG = sig
  (** alias for readability *)
  type block

  val parents : block -> block list
  val children : block -> block list

  (** Protocols may store arbitrary data in the block.
      Think of it as a block header. *)
  type data

  val data : block -> data

  (** Get id of the signer, if the block was signed. *)
  val signature : block -> int option

  (** proof-of-work hash, some protocols use is as source of entropy *)
  type pow

  (** Was the block attached via proof-of-work? *)
  val has_pow : block -> bool

  (** Get proof-of-work hash of the block, if the block was attached via
      proof-of-work. *)
  val pow : block -> pow option

  val max_pow : pow
  val min_pow : pow
  val compare_pow : pow -> pow -> int

  (** raise exception to indicate invalid DAG structure, i.e., bug in simulator
      or protocol spec *)
  val raise_invalid_dag : (block -> Info.t) -> block list -> string -> 'a

  (** Use this to with {!Dagtools.Make} or {!Set.Make} *)
  module Block : Dagtools.Vertex with type t = block
end

type ('a, 'b) blockdag = (module BlockDAG with type block = 'a and type data = 'b)

module type Referee = sig
  include BlockDAG

  (** Restrict BlockDAG extensions. The simulator checks validity for each
      appended block. Invalid blocks are not delivered to other nodes. *)
  val validity : block -> bool

  (** Label block in DAG. *)
  val label : block -> string

  (** Provide debug information. *)
  val info : block -> Info.t

  (** Work spent on the blockchain. Equals block height for
      Nakamoto. Includes uncles for (variants of) Ethereum. Counts votes in
      B_k and Tailstorm.

      DAA tries to maintain constant progress per time. *)
  val progress : block -> float

  (* Coinbase transaction. Assign rewards to participants. *)
  val reward : block -> (int * float) list

  (** Disambiguation in case of forks. The simulator uses this function
      to determine the globally preferred chain. *)
  val winner : block list -> block

  (** Extract the linear history of the given (tip of) chain. Selects one path
      from the (globally preferred) tip of the chain back to the BlockDAG
      roots.

      For Bitcoin, this iterates the blockchain in reverse order. For Ethereum
      it iterates the sequence of blocks w/o uncles in reverse order.

      When calculating accumulated rewards for a blockchain, the simulator uses
      this function to iterate one path in the DAG. Coinbase transactions on
      blocks off this path (e.g. coinbase on Ethereum uncles) are ignored.
  *)
  val precursor : block -> block option
end

type ('a, 'b) referee = (module Referee with type block = 'a and type data = 'b)

module type View = sig
  include BlockDAG

  val my_id : int

  (* Who sees the block? Received implies that block was first received via network.
     Released implies that the block was appended locally and then shared. Withheld
     implies appended locally but not (yet) shared. *)
  val visibility : block -> [ `Received | `Released | `Withheld ]

  (** Since when is the block visible? *)
  val visible_since : block -> float
end

type ('a, 'b) view = (module View with type block = 'a and type data = 'b)

(** Simulator events as they are applied to single network nodes. Something
    becomes visible on the BlockDAG. Either appended on request, received from
    the network, or authorized with proof-of-work. *)
type 'block event =
  | Append of 'block
  | Network of 'block
  | ProofOfWork of 'block

type ('block, 'data) block_draft =
  { parents : 'block list (** hash-references to previous blocks *)
  ; data : 'data (** protocol data stored in the new block *)
  ; sign : bool (** whether to include a signature or not *)
  }

type ('block, 'data, 'state) action =
  { state : 'state (** future state *)
  ; share : 'block list
        (** blocks to be shared with the other nodes. Withheld ancestors are
            shared recursively. *)
  ; append : ('block, 'data) block_draft list (** blocks to be appended to the BlockDAG *)
  }

let return ?(share = []) ?(append = []) state = { state; share; append }

module type Node = sig
  include View

  type state

  (** initialization. The [roots] argument holds references to global versions of
      {protocol.roots}. The roots are visible to all nodes from the beginning. *)
  val init : roots:block list -> state

  (** event handlers *)
  val handler : state -> block event -> (block, data, state) action

  (** [puzzle_payload state] defines the content and parents of the currently mined
      block. When the node solves a proof-of-work puzzle, the simulator calls
      [puzzle_payload], constructs a corresponding block, and hands it to the mining
      node. The simulator raises {Invalid_argument} if the proposed extension does not
      satisfy the DAG invariant specified by the simulated protocol. *)
  val puzzle_payload : state -> (block, data) block_draft

  (** returns a node's preferred block, typically a leave. Rewards are
      calculated backwards from here. *)
  val preferred : state -> block
end

(** we hide the nodes' state type from the simulation. *)
type ('a, 'b) node =
  | Node :
      (module Node with type block = 'a and type data = 'b and type state = 'c)
      -> ('a, 'b) node

module type Protocol = sig
  (** what the protocol stores in each block *)
  type data

  (** a short identifier of the protocol. Used in filenames and as dictionary key. *)
  val key : string

  (** a concise description of the protocol *)
  val description : string

  (** protocol family and parameters *)
  val info : Info.t

  (** specify the BlockDAG roots *)
  val roots : data list

  (** specification of global truths *)
  val referee : ('block, data) blockdag -> ('block, data) referee

  (** specification for honest participants *)
  val honest : ('block, data) view -> ('block, data) node
end

type protocol = Protocol : (module Protocol with type data = 'a) -> protocol

module type AttackSpace = sig
  val key : string
  val info : string

  module Protocol : Protocol

  module Observation : sig
    type t

    val length : int
    val low : floatarray
    val high : floatarray

    (** Encode observation in floatarray of size {!length} for machine observation.
       Result must fall into the range provided by {!low} and {!high}. *)
    val to_floatarray : t -> floatarray

    (** Recover observation from floatarray created with {!to_floatarray}. *)
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

  module Agent (V : View with type data = Protocol.data) : sig
    open V

    type state
    type observable_state

    val preferred : state -> block
    val puzzle_payload : state -> (block, data) block_draft
    val init : roots:block list -> state
    val prepare : state -> block event -> observable_state
    val observe : observable_state -> Observation.t
    val apply : observable_state -> Action.t -> (block, data, state) action
  end

  val policies : (Observation.t -> Action.t) Collection.t

  val attacker
    :  (Observation.t -> Action.t)
    -> ('block, Protocol.data) view
    -> ('block, Protocol.data) node
end

type attack_space =
  | AttackSpace : (module AttackSpace with type Protocol.data = 'a) -> attack_space
