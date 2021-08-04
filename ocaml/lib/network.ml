type link =
  { dest : int
  ; delay : unit -> float
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

let homogeneous ~delay n : t =
  let compute = 1. /. float_of_int n in
  { nodes =
      Array.init n (fun i ->
          { links =
              List.init (n - 1) (fun j -> { dest = (if j >= i then j + 1 else j); delay })
          ; compute
          })
  ; dissemination = Simple
  }
;;
