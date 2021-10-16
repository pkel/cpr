open Rresult

type link =
  { dest : int
  ; delay : float Distributions.iid
  }

type node =
  { compute : float
  ; links : link list
  }

type dissemination =
  | Simple
  | Flooding

type t =
  { nodes : node array
  ; dissemination : dissemination
  ; activation_delay : float
  }

val homogeneous
  :  activation_delay:float
  -> propagation_delay:float Distributions.iid
  -> int
  -> t

type to_graphml =
  ?node_data:(int -> GraphML.Data.t)
  -> ?edge_data:(src:int -> dst:int -> GraphML.Data.t)
  -> ?graph_data:GraphML.Data.t
  -> unit
  -> GraphML.graph

(** write network spec to iGraph/graphml format. *)
val to_graphml : t -> to_graphml

(** Read network spec from iGraph/graphml. The second return value allows to merge results
    back into the source graph. *)
val of_graphml : GraphML.graph -> (t * to_graphml, R.msg) result
