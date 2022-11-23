let version = Version.t

include Intf
module Collection = Collection
module Compare = Compare
module Dag = Dag
module Distributions = Distributions
module GraphML = GraphML
module Info = Info
module Log = Log
module Network = Network
module OrderedQueue = OrderedQueue
module Simulator = Simulator
module ResultSyntax = ResultSyntax

module Infix = struct
  let ( $== ) = Dag.vertex_eq
  let ( $!= ) = Dag.vertex_neq
end

include Infix
