open Rresult

type link =
  { dest : int
  ; delay : float Distributions.iid
  }

type node =
  { compute : float
  ; links : link list
  }

type dissemination =
  | Simple
  | Flooding

type t =
  { nodes : node array
  ; dissemination : dissemination
  ; activation_delay : float
  }

type to_graphml =
  ?node_data:(int -> GraphML.Data.t)
  -> ?edge_data:(src:int -> dst:int -> GraphML.Data.t)
  -> ?graph_data:GraphML.Data.t
  -> unit
  -> GraphML.graph

(** write network spec to iGraph/graphml format. *)
val to_graphml : t -> to_graphml

(** Read network spec from iGraph/graphml. The second return value allows to merge results
    back into the source graph. *)
val of_graphml : GraphML.graph -> (t * to_graphml, R.msg) result

module T : sig
  (** [symmetric_clique n] creates a fully connected network of [n] nodes, all with the
      same share of compute power. *)
  val symmetric_clique
    :  activation_delay:float
    -> propagation_delay:float Distributions.iid
    -> int
    -> t

  (** [two_agents ~alpha] creates a network of two nodes. Node [0] owns [alpha] of the
      compute power. Network delays are zero. *)
  val two_agents : activation_delay:float -> alpha:float -> t

  (** Selfish mining literature uses a single gamma parameter to model network
      connectivity. The attacker sees all defender blocks as soon as they are mined. If
      the attacker decides to release a block of the same height (MATCH action), gamma of
      the defender compute adopts the attackers block.

      In real networks (and in this simulator) honest nodes adopt the block first
      received. Hence, modelling gamma boils down to reordering messages.

      [selfish_mining ~alpha ~gamma ~propagation_delay ~defenders] creates a network that
      exhibits the given [gamma]. The network consists of one attacker node [0] and
      [defenders] honest nodes with symmetrical shares of compute power. Messages between
      defenders exhibits constant [propagation_delay]. Attacker message delays chosen
      randomly to meet [gamma].

      Due to technical reason, [gamma] must be smaller or equal [1 - (1 - alpha) /
      defenders]. *)
  val selfish_mining
    :  alpha:float
    -> activation_delay:float
    -> gamma:float
    -> propagation_delay:float
    -> defenders:int
    -> t
end
