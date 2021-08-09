open Intf

let public_view (v : _ local_view) = { v with view = Dag.filter v.released v.view }

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
  { public : 'env Dag.node (* private/withheld tip of chain *)
  ; private_ : 'env Dag.node (* public/defender tip of chain *)
  }

let withhold
    (honest : ('env, 'data) local_view -> ('env, 'data, 'pow, 'state) node)
    (v : _ local_view)
  =
  let public = honest (public_view v)
  and private_ = honest v in
  let handler actions state event =
    let withhold = { actions with share = (fun _n -> ()) } in
    match event with
    | Activate _ ->
      (* work on private chain *)
      { state with private_ = private_.handler withhold state.private_ event }
      (* NOTE: private_ sees private and public DAG nodes *)
    | Deliver _ ->
      (* simulate defender *)
      { state with public = public.handler withhold state.public event }
  and preferred x = x.private_
  and init ~roots = { public = public.init ~roots; private_ = private_.init ~roots } in
  { init; handler; preferred }
;;

type ('env, 'data) tactic =
  ('env, 'data) local_view
  -> release:('env Dag.node -> unit)
  -> 'env state
  -> [ `Abort | `Continue ]

let apply_tactic (tactic : ('env, 'data) tactic) (v : _ local_view) actions state =
  tactic v ~release:actions.share state
  |> function
  | `Continue -> state
  | `Abort -> { state with private_ = state.public }
;;

let attack
    (honest : ('env, 'data) local_view -> ('env, 'data, 'pow, 'state) node)
    (tactic : ('env, 'data) tactic)
    (v : _ local_view)
  =
  let node = withhold honest v in
  let handler actions state event =
    node.handler actions state event |> apply_tactic tactic v actions
  in
  { node with handler }
;;
