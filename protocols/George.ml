open Cpr_lib
open Protocol

type height =
  { block : int
  ; vote : int
  }

let is_vote h = h.vote > 0
let is_block h = h.vote = 0

(* TODO BUG: more than k pows might be referenced. Check nested references. *)
(* TODO verify vote uniqueness *)
let dag_validity ~k (v : _ global_view) n =
  let child = v.data n in
  child.block >= 0
  && child.vote >= 0
  && child.vote < k
  && v.pow_hash n |> Option.is_some
  &&
  match v.data n |> is_vote, Dag.parents v.view n with
  | true, [ p ] ->
    (* child is vote *)
    let parent = v.data p in
    child.vote = parent.vote + 1
  | false, p :: votes ->
    let parent = v.data p in
    (* child is block *)
    is_block parent
    && List.for_all (fun n -> v.data n |> is_vote) votes
    && List.length votes = k - 1
    && child.block = parent.block + 1
    && child.vote = 0
  | _ -> false
;;

let dag_roots = [ { block = 0; vote = 0 } ]

let init ~roots =
  match roots with
  | [ genesis ] -> genesis
  | _ -> failwith "invalid roots"
;;

type ('env, 'data) extended_view =
  { view : 'env Dag.view
  ; data : 'env Dag.node -> 'data
  ; votes_only : 'env Dag.view
  ; blocks_only : 'env Dag.view
  ; delivered_at : 'env Dag.node -> float
  ; appended_by_me : 'env Dag.node -> bool
  ; my_id : int
  }

let extend_view (x : _ Protocol.local_view) =
  { view = x.view
  ; data = x.data
  ; votes_only = Dag.filter (fun n -> x.data n |> is_vote) x.view
  ; blocks_only = Dag.filter (fun n -> x.data n |> is_block) x.view
  ; delivered_at = x.delivered_at
  ; appended_by_me = x.appended_by_me
  ; my_id = x.my_id
  }
;;

let rec last_block v n =
  if v.data n |> is_block
  then n
  else (
    match Dag.parents v.view n with
    | [ gnode ] -> last_block v gnode
    | _ -> failwith "invalid dag" (* votes have only one parent by dag_validity *))
;;

let first by n l =
  let a = Array.of_list l in
  if Array.length a < n then raise (Invalid_argument "list too short");
  let () = Array.sort (fun a b -> compare (by a) (by b)) a in
  let l = ref [] in
  for i = 0 to n - 1 do
    l := a.(i) :: !l
  done;
  !l
;;

(* recursive version of children *)
let offspring view node =
  let l = ref [] in
  let rec f node =
    Dag.children view node
    |> List.iter (fun c ->
           l := c :: !l;
           f c)
  in
  f node;
  !l
;;

let honest ~k v =
  let v = extend_view v in
  let handler actions preferred = function
    | Activate pow ->
      let b = last_block v preferred in
      let votes = offspring v.votes_only b in
      if List.length votes >= k - 1
      then (
        let b =
          actions.extend_dag
            ~pow
            (* TODO Bug: transitive closure of last k received votes can be bigger than k *)
            (b :: first v.delivered_at (k - 1) votes)
            { block = (v.data b).block + 1; vote = 0 }
        in
        let () = actions.share b in
        b)
      else (
        let vd = v.data preferred in
        let v = actions.extend_dag ~pow [ preferred ] { vd with vote = vd.vote + 1 } in
        let () = actions.share v in
        v)
    | Deliver consider ->
      (* Prefer longest chain of votes after longest chain of blocks *)
      let p = v.data preferred
      and c = v.data consider in
      if c.block > p.block || (c.block = p.block && c.vote > p.vote)
      then consider
      else preferred
  in
  Node { init; handler; preferred = (fun x -> x) }
;;

let protocol ~k = { honest = honest ~k; dag_validity = dag_validity ~k; dag_roots }

let%test "convergence" =
  let open Simulator in
  let test k params height =
    let protocol = protocol ~k in
    init params protocol
    |> loop params
    |> fun { nodes; global; _ } ->
    Array.to_seq nodes
    |> Seq.map (fun (SNode x) -> x.preferred x.state)
    |> Dag.common_ancestor'
         (Dag.filter (fun x -> is_block (Dag.data x).value) global.view)
    |> function
    | None -> false
    | Some n ->
      let bheight = (Dag.data n).value.block in
      if bheight < height
      then (
        Printf.eprintf "k: %i\theight: %i\texpected: %i\n" k bheight height;
        false)
      else true
    (* more than 900 blocks in a sequence imply less than 10% orphans. *)
  in
  let delay = Distributions.exponential ~ev:1. in
  List.for_all
    (fun (k, activation_delay, height) ->
      let network = Network.homogeneous ~delay 32 in
      test k { network; activations = 1000 * k; activation_delay } height)
    [ 08, 10., 900 (* good condition, 10% orphans *)
    ; 08, 01., 700 (* bad conditions, 30% orphans *)
    ; 32, 01., 900
      (* bad conditions, 10% orphans, high k *)
    ]
;;

let constant_block c : _ reward_function =
 fun ~view:v ~assign n -> if v.data n |> is_block then assign c n
;;

let reward ~max_reward_per_block ~discount ~punish ~k : ('env, height) reward_function =
  let k = float_of_int k in
  let c = max_reward_per_block /. k in
  fun ~view:v ~assign ->
    let vote_view = Dag.filter (fun x -> v.data x |> is_vote) v.view in
    fun n ->
      if v.data n |> is_block
      then (
        match Dag.parents vote_view n with
        | [] -> (* Either genesis or k=1 *) assign c n
        | hd :: tl as votes ->
          let get_vdepth n = (v.data n).vote in
          let longest, depth =
            List.fold_left
              (fun acc v ->
                let depth = get_vdepth v in
                if depth > snd acc then v, depth else acc)
              (hd, get_vdepth hd)
              tl
          in
          let x = if discount then (float_of_int depth +. 1.) /. k *. c else c in
          assign x n;
          if punish
          then
            (* TODO BUG. longest can be ambiguous *)
            Dag.iterate_ancestors vote_view [ longest ] |> Seq.iter (assign x)
          else List.iter (assign x) votes)
;;

(* TODO: add tests for reward functions *)
