type ('env, 'dag_data, 'pow) actions = ('env, 'dag_data, 'pow) Intf.actions =
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
type ('env, 'pow) event = ('env, 'pow) Intf.event =
  | Activate of 'pow
  | Deliver of 'env Dag.vertex

module type EnvTypes = sig
  (** what the simulator stores on each DAG vertex *)
  type env

  (** how the simulator models proof-of-work puzzle solutions *)
  type pow
end

module type ProtocolTypes = sig
  (** what the protocol stores on each DAG vertex *)
  type data
end

module type GlobalView = sig
  include EnvTypes
  include ProtocolTypes

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
end

type ('a, 'b, 'c) local_view =
  (module LocalView with type env = 'a and type data = 'b and type pow = 'c)

module type Node = sig
  include LocalView

  type state

  (** initialization. The [roots] argument holds references to global versions of
      {protocol.dag_roots}. The roots are visible to all nodes from the beginning. *)
  val init : roots:env Dag.vertex list -> state

  (** event handlers. May trigger side effects via [actions] argument. *)
  val handler : (env, data, pow) actions -> state -> (env, pow) event -> state

  (* TODO: actions.extend_dag could be part of the local view, actions.share could be
     replaced by returning to be shared vertices from the hander *)

  (** returns a node's preferred tip of the chain. *)
  val preferred : state -> env Dag.vertex
end

type ('a, 'b, 'c, 'd) node =
  (module Node with type env = 'a and type data = 'b and type pow = 'c and type state = 'd)

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

  module Make (Env : EnvTypes) : sig
    open Env

    (** specify the roots of the DAG. *)
    val dag_roots : data list

    (** restrict DAG extensions. The simulator checks validity for each appended DAG
        vertex. Invalid extensions are not delivered to other nodes. *)
    val dag_validity : (env, data) global_view -> env Dag.vertex -> bool

    type node' =
      | Node : (env, data, pow, 'state) node -> node'
          (** we use this to hide the node's state from the simulator. It also enables
              different node implementations operating on different local state but in the
              same simulation. *)

    val honest : (env, data, pow) local_view -> node'
    val reward_functions : (env, data) Intf.reward_function Collection.t

    (* TODO, eliminate Intf dependency *)

    val attacks : ((env, data, pow) local_view -> node') Collection.t
  end
end
