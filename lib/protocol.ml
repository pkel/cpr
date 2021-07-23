type ('env, 'data, 'pow) actions =
  { share : 'env Dag.node -> unit
        (** Instruct the simulator to make the DAG node visible to other network nodes.
            The simulator might apply network delays depending on its configuration. *)
  ; extend_dag : ?pow:'pow -> ?sign:bool -> 'env Dag.node list -> 'data -> 'env Dag.node
        (** [extend_dag ~pow ~sign parents data] adds a node with [data] to the
            simulator's DAG. Initially, only the extending network node can see the new
            node. The simulator raises {Invalid_argument} if the proposed extension does
            not satisfy the DAG invariant specified by the simulated protocol. *)
  }

(** Simulator events as they are applied to single network nodes *)
type ('env, 'pow) event =
  | Activate of 'pow
  | Deliver of 'env Dag.node

(** Behaviour of a single network node. Type of node local state is packed. *)
type ('env, 'data, 'pow) node =
  | Node :
      { init : roots:'env Dag.node list -> 'state
            (** Node initialization. The [roots] argument holds references to global
                versions of {protocol.dag_roots}. The roots are visible to all nodes from
                the beginning. *)
      ; handler : ('env, 'data, 'pow) actions -> 'state -> ('env, 'pow) event -> 'state
            (** Event handlers. May trigger side effects via [actions] argument. *)
      ; preferred : 'state -> 'env Dag.node
            (** Returns a node's preferred tip of the chain. *)
      }
      -> ('env, 'data, 'pow) node

type ('env, 'data) global_view =
  { view : 'env Dag.view (** View on the simulator's DAG. *)
  ; data : 'env Dag.node -> 'data (** Read the protocol data attached to DAG node. *)
  ; signed_by : 'env Dag.node -> int option
        (** Return id of signer if DAG node was signed. *)
  ; pow_hash : 'env Dag.node -> (int * int) option
        (** Return PoW hash of node, if node was attached with PoW authorization. *)
  }

type ('env, 'data) local_view =
  { view : 'env Dag.view
        (** Restricted view on the simulator's DAG models the information set of network
            nodes. *)
  ; data : 'env Dag.node -> 'data (** Read the protocol data attached to DAG node. *)
  ; signed_by : 'env Dag.node -> int option
        (** Return id of signer if DAG node was signed. *)
  ; my_id : int
  ; pow_hash : 'env Dag.node -> (int * int) option
        (** Return PoW hash of node, if node was attached with PoW authorization. *)
  ; delivered_at : 'env Dag.node -> float (** Get time of delivery of DAG nodes. *)
  ; released : 'env Dag.node -> bool (** Was this node already shared? *)
  ; appended_by_me : 'env Dag.node -> bool (** Recognize own DAG nodes. *)
  }

type ('env, 'data, 'pow) protocol =
  { dag_roots : 'data list (** Specify the roots of the global DAG. *)
  ; dag_validity : ('env, 'data) global_view -> 'env Dag.node -> bool
        (** Restrict DAG extensions. The simulator checks validity for each appended DAG
            node. Invalid extensions are not delivered to other nodes. *)
  ; honest : ('env, 'data) local_view -> ('env, 'data, 'pow) node
  }

(** Calculate and assign rewards to a nodes and (potentially) its neighbours. Use this
    together with {!Dag.iterate_ancestors}. *)
type ('env, 'data) reward_function =
  view:('env, 'data) global_view
  -> assign:(float -> 'env Dag.node -> unit)
  -> 'env Dag.node
  -> unit
