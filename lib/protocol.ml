type ('env, 'data, 'pow) context =
  { view : 'env Dag.view
        (** View on the simulator's DAG. Partial visibility models the information set of
            the network node. *)
  ; read : 'env -> 'data
        (** Read the protocol data from simulator data attached to DAG nodes. *)
  ; share : 'env Dag.node -> unit
        (** Instruct the simulator to make the DAG node visible to other network nodes.
            The simulator might apply network delays depending on its configuration. *)
  ; extend_dag : ?pow:'pow -> 'env Dag.node list -> 'data -> 'env Dag.node
        (** [extend_dag ~pow parents data] adds a node with [data] to the simulator's DAG.
            Initially, only the extending network node can see the new node. The simulator
            raises {Invalid_argument} if the proposed extension does not satisfy the DAG
            invariant specified by the simulated protocol. *)
  }

(** Simulator events as they are applied to single network nodes *)
type ('env, 'pow) event =
  | Activate of 'pow
  | Deliver of 'env Dag.node

(** Behaviour of a single network node. Functions can trigger side-effects via {context}. *)
type ('env, 'state, 'pow) implementation =
  { handler : 'state -> ('env, 'pow) event -> 'state
  ; init : roots:'env Dag.node list -> 'state
        (** The [roots] argument holds references to global versions of the {dag_roots} of
            the {protocol}. The roots are visible to all nodes from the beginning. *)
  }

type ('env, 'data, 'state, 'pow) protocol =
  { dag_roots : 'data list (** Specify the roots of the global DAG. *)
  ; dag_invariant : pow:bool -> 'data list -> 'data -> bool
        (** Restrict the set of valid DAGs. The simulator checks [dag_invariant ~pow
            parents data] for each extension proposed by network nodes via
            {Context.extend_dag}. Extension validity can depend on the proof-of-work
            authorization, parent data, and extension data. *)
  ; spawn : ('env, 'data, 'pow) context -> ('env, 'state, 'pow) implementation
        (** honest network nodes' behaviour *)
  ; head : 'state -> 'env Dag.node (** Preferred DAG node. *)
  }

(** Calculate and assign rewards to nodes from the roots of the DAG to the given node. *)
type ('env, 'data) reward_function =
  (* see context.view *) 'env Dag.view
  -> (* see context.read *) ('env -> 'data)
  -> (* which node has attached the node? None if environmental node, e.g. root *)
     ('env -> int option)
  -> (* head of the DAG/chain. Rewards are calculated for the complete history. *)
     'env Dag.node
  -> (* rewards will be added to the floats in this array *) floatarray
  -> unit
