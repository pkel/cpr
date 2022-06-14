open Cpr_lib.Next
include Nakamoto0

let attacks () =
  let open Nakamoto_SSZ in
  Collection.map
    (fun { key; info; it } ->
      { key = "ssz-" ^ key; info = info ^ "; " ^ info; it = agent it })
    policies
;;

module SSZ_attack = struct
  module Protocol = struct
    include Nakamoto0

    let attacks = attacks
  end

  type data = Protocol.data

  include Nakamoto_SSZ
end

let attack_spaces : (_ * data attack_space) list = [ "ssz", (module SSZ_attack) ]
