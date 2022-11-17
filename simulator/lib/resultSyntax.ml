open Result

let ( let+ ) a b = map b a
let ( let* ) = bind

let pair a b =
  let* a = a in
  let+ b = b in
  a, b
;;

let ( and+ ) = pair
let ( and* ) = pair
