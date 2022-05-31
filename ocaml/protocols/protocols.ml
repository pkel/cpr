module Nakamoto = Nakamoto
module B_k_lessleader = B_k_lessleader
module B_k = B_k
module Tailstorm = Tailstorm

module Next = struct
  open Cpr_lib.Next
  module Nakamoto : Protocol = Nakamoto
end
