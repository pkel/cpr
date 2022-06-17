open Cpr_protocols

let nakamoto = Engine.of_module (module Nakamoto.SSZattack)

let bk ~k =
  let module Bk =
    Bk.Make (struct
      let k = k
    end)
  in
  Engine.of_module (module Bk.SSZattack)
;;
