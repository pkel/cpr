exception Invalid_dag_extension

type ('global, 'local, 'pow) context =
  { view : ('global, 'local) Dag.view
  ; release : 'global Dag.node -> unit
  ; extend_dag : ?pow:'pow -> 'global Dag.node list -> 'local -> 'global Dag.node
  }

type ('global, 'pow) event =
  | Activate of 'pow
  | Deliver of 'global Dag.node

type ('global, 'local, 'state, 'pow) protocol =
  { event_handler :
      ('global, 'local, 'pow) context -> 'state -> ('global, 'pow) event -> 'state
  ; init : roots:'global Dag.node list -> 'state
  ; dag_roots : 'local list
  ; dag_invariant : pow:bool -> parents:'local list -> child:'local -> bool
  }
