open Cpr_lib.Next

module type Parameters = Bk0.Parameters

module Make (P : Parameters) = struct
  include Bk0.Make (P)
  module SSZattack = Bk_ssz.Make (P)

  let attack_spaces : data attack_space list = [ (module SSZattack) ]
end
