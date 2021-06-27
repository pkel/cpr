type link =
  { dest : int
  ; delay : unit -> float
  }

type node =
  { links : link list
  ; compute : float
  }

type t = node array

let homogeneous ~delay n =
  let compute = 1. /. float_of_int n in
  Array.init n (fun i ->
      { links =
          List.init (n - 1) (fun j -> { dest = (if j >= i then j + 1 else j); delay })
      ; compute
      })
;;
