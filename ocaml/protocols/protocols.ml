module Nakamoto = Nakamoto
module B_k_lessleader = B_k_lessleader
module B_k = B_k
module Tailstorm = Tailstorm

module Next = struct
  open Cpr_lib.Next

  module Nakamoto : Protocol = struct
    include Nakamoto

    type data = dag_data

    let key = "nakamoto"
    let info = "Nakamoto consensus"

    let judge (type a) (v : (a, data) global_view) l =
      let (module V) = v in
      List.to_seq l |> Cpr_lib.Dag.common_ancestor' V.view |> Option.get
    ;;
  end
end
