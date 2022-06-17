open Cpr_lib.Next
include Nakamoto0
module SSZattack = Nakamoto_ssz

let attack_spaces : data attack_space list = [ (module SSZattack) ]
