open Cpr_lib.Next

module type Parameters = Bk0.Parameters

include Bk0.Data

module Make (P : Parameters) = struct
  include Bk0.Make (P)

  let attacks () = Collection.empty
  let attack_spaces = Collection.empty
end

let make ~k =
  (module Make (struct
    let k = k
  end) : Protocol
    with type data = data)
;;
