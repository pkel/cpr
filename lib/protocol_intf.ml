module type CONTEXT = sig
  (** Simulator data attached to each node in the DAG *)
  type env

  (** Protocol data attached to each node in the DAG *)
  type data

  (** Opaque witness for having solved a proof-of-work puzzle. Each witness can used for at most one extension of the DAG. *)
  type pow

  (** Simulator events as they are applied to single network nodes *)
  type event =
    | Activate of pow
    | Deliver of env Dag.node

  (** View on the simulator's DAG. Partial visibility models the information set of the network node. *)
  val view : env Dag.view

  (** Read the protocol data from simulator data attached to DAG nodes. *)
  val read : env -> data

  (** [extend_dag ~pow parents data] adds a node with [data] to the simulator's DAG. Initially, only the extending network node can see the new node. The simulator raises {Invalid_argument} if the proposed extension does not satisfy the DAG invariant specified by the simulated protocol. *)
  val extend_dag : ?pow:pow -> env Dag.node list -> data -> env Dag.node

  (** Instruct the simulator to make the DAG node visible to other network nodes. The simulator might apply network delays depending on its configuration. *)
  val share : env Dag.node -> unit
end

module type PROTOCOL = sig
  (** Protocol data attached to each node in the DAG *)
  type data

  (** Specify the roots of the global DAG. *)
  val dag_roots : data list

  (** Restrict the set of valid DAGs. The simulator checks [dag_invariant ~pow parents data] for each extension proposed by network nodes via {Context.extend_dag}. Extension validity can depend on the proof-of-work authorization, parent data, and extension data. *)
  val dag_invariant : pow:bool -> data list -> data -> bool

  (** State of a single network node *)
  type 'a state

  module Spawn (CTX : CONTEXT with type data := data) : sig
    (** Spawn an honest network node. *)
    open CTX

    (** Behaviour of honest network nodes. Can trigger side-effects via [CTX]. *)
    val handler : env state -> event -> env state

    (** Initialization of honest network nodes. The [roots] argument holds references to global versions of the {dag_roots}. The roots are visible to all nodes from the beginning. *)
    val init : roots:env Dag.node list -> env state
  end
end
