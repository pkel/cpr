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
