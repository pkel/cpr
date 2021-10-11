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
  }

val homogeneous : delay:float Distributions.iid -> int -> t

(** read network spec from iGraph/graphml *)
val of_graphml : GraphML.graph -> (t, t * string list) result
