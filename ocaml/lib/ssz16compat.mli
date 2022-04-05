open Intf

type 'env state = private
  { public : 'env Dag.vertex (* private/withheld tip of chain *)
  ; private_ : 'env Dag.vertex (* public/defender tip of chain *)
  ; common : 'env Dag.vertex (* tip of common chain *)
  }

val public_view
  :  'env state
  -> ('env, 'dag_data) local_view
  -> ('env, 'dag_data) local_view

val private_view
  :  'env state
  -> ('env, 'dag_data) local_view
  -> ('env, 'dag_data) local_view

type 'env action =
  { release : 'env Dag.vertex option (* release this vertex and all dependencies *)
  ; adopt : bool (* adopt public chain and discard private blocks *)
  }

type ('env, 'dag_data) tactic = ('env, 'dag_data) local_view -> 'env state -> 'env action

val attack
  :  honest:('env, 'dag_data, 'pow, 'env Dag.vertex) node
  -> ('env, 'dag_data) tactic
  -> ('env, 'dag_data) local_view
  -> ('env, 'dag_data, 'pow, 'env state) node'

(* [attack] decomposed into two steps for the ML framework: *)

val withhold
  :  honest:('env, 'dag_data, 'pow, 'env Dag.vertex) node
  -> ('env, 'dag_data) local_view
  -> ('env, 'dag_data, 'pow, 'env state) node'

val apply_tactic
  :  honest:('env, 'dag_data, 'pow, 'env Dag.vertex) node
  -> ('env, 'dag_data) tactic
  -> ('env, 'dag_data) local_view
  -> ('env, 'dag_data, 'pow) Intf.actions
  -> 'env state
  -> 'env state
