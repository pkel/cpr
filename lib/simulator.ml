type 'a data =
  { value : 'a
  ; visibility : bool array (* could be a bitfield *)
  }

type 'a event =
  | Activation
  | Deliver of 'a data Dag.node

type ('a, 'b) node =
  { preferred : 'a data Dag.node
  ; state : 'b
  }

type ('a, 'b) state =
  { tree : 'a Dag.t
  ; queue : (float, 'a event) OrderedQueue.t
  ; nodes : ('a, 'b) node array
  }
