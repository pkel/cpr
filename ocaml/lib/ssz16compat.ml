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

let set_common v state =
  let common = Dag.common_ancestor v.view state.public state.private_ in
  assert (Option.is_some common) (* all our protocols maintain this invariant *);
  { state with common = Option.get common }
;;

let handle_private ~honest v actions state event =
  let node = honest (private_view state v)
  and drop_messages = { actions with share = (fun _n -> ()) } in
  set_common v { state with private_ = node.handler drop_messages state.private_ event }
;;

let handle_public ~honest v actions state event =
  let node = honest (public_view state v)
  and drop_messages = { actions with share = (fun _n -> ()) } in
  set_common v { state with public = node.handler drop_messages state.public event }
;;

(* TODO: for non-Nakamoto protocols, we might consider delivering some of the messages to
   the attacker. E.g. in B_k, the attacker might try to fork but still use defender votes.
   Not sure whether this is supported by this module. *)
let handler ~honest v actions state event =
  match event with
  | Activate _ ->
    (* work on private chain *)
    handle_private ~honest v actions state event
  | Deliver _ ->
    (* simulate defender *)
    handle_public ~honest v actions state event
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

(* release a given vertex and all its dependencies recursively *)
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
  let state =
    (* release information and simulate defender *)
    match release with
    | None -> state
    | Some release ->
      let () = release_recursive v actions.share [ release ] in
      handle_public ~honest v actions state (Deliver release)
  in
  (* adopt defender chain (or don't) *)
  if adopt then { state with private_ = state.public; common = state.public } else state
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
