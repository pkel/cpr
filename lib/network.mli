type link =
  { dest : int
  ; delay : float Distributions.iid
  }

type node =
  { links : link list
  ; compute : float
  }

type t = node array

val homogeneous : delay:float Distributions.iid -> int -> t
