open Intf

type 'env state =
  { public : 'env Dag.vertex
  ; private_ : 'env Dag.vertex
  ; common : 'env Dag.vertex
  }

let private_view state (v : _ local_view) =
  let visible vertex =
    Dag.partial_order state.common vertex > 0 || v.appended_by_me vertex
  in
  { v with view = Dag.filter visible v.view }
;;

let public_view state (v : _ local_view) =
  let visible vertex =
    Dag.partial_order state.common vertex > 0 || not (v.appended_by_me vertex)
  in
  { v with
    view = Dag.filter visible v.view
  ; appended_by_me =
      (* The attacker simulates an honest node on the public view. This node should not
         interpret attacker vertices as own vertices. *)
      (fun _ -> false)
  }
;;

let update_common v state =
  let common = Dag.common_ancestor v.view state.public state.private_ in
  assert (Option.is_some common) (* all our protocols maintain this invariant *);
  { state with common = Option.get common }
;;

let handler ~honest v actions state event =
  let public = honest (public_view state v)
  and private_ = honest (private_view state v) in
  let withhold = { actions with share = (fun _n -> ()) } in
  match event with
  | Activate _ ->
    (* work on private chain *)
    { state with private_ = private_.handler withhold state.private_ event }
  | Deliver _ ->
    (* simulate defender *)
    let public = public.handler withhold state.public event in
    (* TODO: for non-Nakamoto protocols, we might consider delivering some of the messages
       to the attacker. E.g. in B_k, the attacker might try to fork but still use defender
       votes. Not sure whether this is supported by this module. *)
    update_common v { state with public }
;;

let withhold ~(honest : ('env, 'dag_data, 'pow, 'state) node) (v : _ local_view) =
  let handler = handler ~honest v
  and preferred x = x.private_
  and init ~roots =
    let v = (honest v).init ~roots in
    { public = v; private_ = v; common = v }
  in
  { init; handler; preferred }
;;

type 'env action =
  { release : 'env Dag.vertex option (* release this vertex and all dependencies *)
  ; adopt : bool (* adopt public chain and discard private blocks *)
  }

type ('env, 'dag_data) tactic = ('env, 'dag_data) local_view -> 'env state -> 'env action

(* release a given vertex and all it's dependencies recursively *)
let rec release_recursive v release ns =
  List.iter
    (fun n ->
      if not (v.released n)
      then (
        release n;
        release_recursive v release (Dag.parents v.view n)))
    ns
;;

let apply_tactic
    ~(honest : ('env, 'dag_data, 'pow, 'state) node)
    (tactic : ('env, 'dag_data) tactic)
    (v : _ local_view)
    actions
    state
  =
  let { release; adopt } = tactic v state in
  let public =
    match release with
    | None -> state.public
    | Some release ->
      let () = release_recursive v actions.share [ release ] in
      let withhold = { actions with share = (fun _n -> ()) } in
      let public = honest (public_view state v) in
      public.handler withhold state.public (Deliver release)
  in
  let private_ = if adopt then public else state.private_ in
  let common = Dag.common_ancestor v.view public private_ in
  assert (Option.is_some common) (* all our protocols maintain this invariant *);
  { private_; public; common = Option.get common }
;;

let attack
    ~(honest : ('env, 'dag_data, 'pow, 'state) node)
    (tactic : ('env, 'dag_data) tactic)
    (v : _ local_view)
  =
  let node = withhold ~honest v in
  let handler actions state event =
    node.handler actions state event |> apply_tactic ~honest tactic v actions
  in
  { node with handler }
;;
