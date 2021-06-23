exception Invalid_dag_extension

type ('env, 'data, 'pow) context =
  { view : 'env Dag.view
  ; data : 'env Dag.node -> 'data
  ; release : 'env Dag.node -> unit
  ; extend_dag : ?pow:'pow -> 'env Dag.node list -> 'data -> 'env Dag.node
  }

type ('env, 'pow) event =
  | Activate of 'pow
  | Deliver of 'env Dag.node

type ('env, 'data, 'state, 'pow) protocol =
  { event_handler : ('env, 'data, 'pow) context -> 'state -> ('env, 'pow) event -> 'state
  ; init : roots:'env Dag.node list -> 'state
  ; dag_roots : 'data list
  ; dag_invariant : pow:bool -> parents:'data list -> child:'data -> bool
  }
