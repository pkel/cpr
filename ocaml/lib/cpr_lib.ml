let version = Version.t

include Intf
module Collection = Collection
module Compare = Compare
module Dag = Dag
module Distributions = Distributions
module GraphML = GraphML
module Network = Network
module Simulator = Simulator
module ResultSyntax = ResultSyntax

module Infix = struct
  let ( $== ) = Dag.vertex_eq
  let ( $!= ) = Dag.vertex_neq
end

include Infix
