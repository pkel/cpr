open Cpr_protocols

let nakamoto =
  Engine.of_module
    (module struct
      module Protocol = Nakamoto
      include Protocol.SszAttack
    end)
;;
