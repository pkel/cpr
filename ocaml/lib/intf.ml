type ('env, 'dag_data, 'pow) actions =
  { share : ?recursive:bool -> 'env Dag.vertex -> unit
        (** Instruct the simulator to make the DAG vertex visible to other network nodes.
            The simulator might apply network delays depending on its configuration.

            The recursive flag controls whether the dependencies are released as well;
            default: false. *)
  ; extend_dag :
      ?pow:'pow -> ?sign:bool -> 'env Dag.vertex list -> 'dag_data -> 'env Dag.vertex
        (** [extend_dag ~pow ~sign parents data] adds a vertex with [data] to the
            simulator's DAG. Initially, only the extending network node can see the new
            vertex. The simulator raises {Invalid_argument} if the proposed extension does
            not satisfy the DAG invariant specified by the simulated protocol. *)
  }

(** Simulator events as they are applied to single network nodes *)
type ('env, 'pow) event =
  | Activate of 'pow
  | Deliver of 'env Dag.vertex

type ('env, 'dag_data) global_view =
  { view : 'env Dag.view (** View on the simulator's DAG. *)
  ; data : 'env Dag.vertex -> 'dag_data
        (** Read the protocol data attached to DAG vertex. *)
  ; signed_by : 'env Dag.vertex -> int option
        (** Return id of signer if DAG vertex was signed. *)
  ; pow_hash : 'env Dag.vertex -> (int * int) option
        (** Return PoW hash of vertex, if vertex was attached with PoW authorization. *)
  }

type ('env, 'dag_data) local_view =
  { view : 'env Dag.view
        (** Restricted view on the simulator's DAG models the information set of network
            nodes. *)
  ; data : 'env Dag.vertex -> 'dag_data
        (** Read the protocol data attached to DAG vertex. *)
  ; signed_by : 'env Dag.vertex -> int option
        (** Return id of signer if DAG vertex was signed. *)
  ; my_id : int
  ; pow_hash : 'env Dag.vertex -> (int * int) option
        (** Return PoW hash of vertex, if vertex was attached with PoW authorization. *)
  ; delivered_at : 'env Dag.vertex -> float (** Get time of delivery of DAG vertices. *)
  ; released : 'env Dag.vertex -> bool
        (* TODO: remove this field after getting of the old PrivateAttack module *)
        (** Was this vertex already shared? *)
  ; appended_by_me : 'env Dag.vertex -> bool (** Recognize own DAG vertices. *)
  }

(** Behaviour of a single network node. *)
type ('env, 'dag_data, 'pow, 'state) node' =
  { init : roots:'env Dag.vertex list -> 'state
        (** Node initialization. The [roots] argument holds references to global versions
            of {protocol.dag_roots}. The roots are visible to all nodes from the
            beginning. *)
  ; handler : ('env, 'dag_data, 'pow) actions -> 'state -> ('env, 'pow) event -> 'state
        (** Event handlers. May trigger side effects via [actions] argument. *)
  ; preferred : 'state -> 'env Dag.vertex
        (** Returns a node's preferred tip of the chain. *)
  }

type ('env, 'dag_data, 'pow, 'state) node =
  ('env, 'dag_data) local_view -> ('env, 'dag_data, 'pow, 'state) node'

type ('env, 'dag_data, 'pow) opaque_node =
  | Node : ('env, 'dag_data, 'pow, 'state) node -> ('env, 'dag_data, 'pow) opaque_node

(** Calculate and assign rewards to a vertex and (potentially) its neighbours. Use this
    together with {!Dag.iterate_ancestors}. *)
type ('env, 'dag_data) reward_function =
  view:('env, 'dag_data) global_view
  -> assign:(float -> 'env Dag.vertex -> unit)
  -> 'env Dag.vertex
  -> unit

type ('env, 'dag_data, 'pow, 'state) protocol =
  { key : string
  ; info : string
  ; pow_per_block : int
  ; dag_roots : 'dag_data list (** Specify the roots of the global DAG. *)
  ; dag_validity : ('env, 'dag_data) global_view -> 'env Dag.vertex -> bool
        (** Restrict DAG extensions. The simulator checks validity for each appended DAG
            vertex. Invalid extensions are not delivered to other nodes. *)
  ; honest : ('env, 'dag_data, 'pow, 'state) node
  ; describe : 'dag_data -> string
  ; height : 'dag_data -> int
  ; reward_functions : ('env, 'dag_data) reward_function Collection.t
  ; attacks : ('env, 'dag_data, 'pow) opaque_node Collection.t
  }
