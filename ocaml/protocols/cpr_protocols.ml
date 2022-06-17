module Nakamoto = struct
  include Nakamoto
  module SSZattack = Nakamoto_ssz
end

module Bk = struct
  module Make (P : Bk.Parameters) = struct
    include Bk.Make (P)
    module SSZattack = Bk_ssz.Make (P)
  end
end

(** deprecated / old interface *)

module B_k_lessleader = B_k_lessleader
module Tailstorm = Tailstorm
