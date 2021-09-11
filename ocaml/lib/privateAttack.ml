open Intf

let public_view (v : _ local_view) =
  { v with
    view = Dag.filter v.released v.view
  ; appended_by_me =
      (* avoid simulated public node appending Bₖ blocks in the name of the attacker *)
      (fun _ -> false)
  }
;;

(* release a given node and all it's dependencies recursively *)
let rec release_recursive v release ns =
  List.iter
    (fun n ->
      if not (v.released n)
      then (
        release n;
        release_recursive v release (Dag.parents v.view n)))
    ns
;;

type 'env state =
  { public : 'env Dag.vertex (* private/withheld tip of chain *)
  ; private_ : 'env Dag.vertex (* public/defender tip of chain *)
  }

let withhold (honest : ('env, 'dag_data, 'pow, 'state) node) (v : _ local_view) =
  let public = honest (public_view v)
  and private_ = honest v in
  let handler actions state event =
    let withhold = { actions with share = (fun _n -> ()) } in
    match event with
    | Activate _ ->
      (* work on private chain *)
      { state with private_ = private_.handler withhold state.private_ event }
    | Deliver _ ->
      (* simulate defender *)
      (* NOTE: This logic implies that the attacker adopts the longest public chain if it
         is better. Consider adding a third tip of chain that ignores public information. *)
      { public = public.handler withhold state.public event
      ; private_ = private_.handler withhold state.private_ event
      }
  and preferred x = x.private_
  and init ~roots = { public = public.init ~roots; private_ = private_.init ~roots } in
  { init; handler; preferred }
;;

type ('env, 'dag_data) tactic =
  ('env, 'dag_data) local_view
  -> release:('env Dag.vertex -> unit)
  -> 'env state
  -> [ `PreferPublic | `PreferPrivate ]

let apply_tactic (tactic : ('env, 'dag_data) tactic) (v : _ local_view) actions state =
  tactic v ~release:actions.share state
  |> function
  | `PreferPrivate -> state
  | `PreferPublic -> { state with private_ = state.public }
;;

let attack
    (honest : ('env, 'dag_data, 'pow, 'state) node)
    (tactic : ('env, 'dag_data) tactic)
    (v : _ local_view)
  =
  let node = withhold honest v in
  let handler actions state event =
    node.handler actions state event |> apply_tactic tactic v actions
  in
  { node with handler }
;;
