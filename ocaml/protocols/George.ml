open Cpr_lib

type dag_data =
  { block : int
  ; vote : int
  }

let is_vote h = h.vote > 0
let is_block h = h.vote = 0

let describe h =
  let ty = if is_vote h then "vote" else "block" in
  Printf.sprintf "%s (%i|%i)" ty h.block h.vote
;;

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
    child.block = parent.block && child.vote = parent.vote + 1
  | false, p :: votes ->
    (* child is block *)
    let parent = v.data p in
    let unique_votes =
      let votes_only = Dag.filter (fun n -> v.data n |> is_vote) v.view in
      Dag.iterate_ancestors votes_only votes |> Seq.fold_left (fun acc _ -> acc + 1) 0
    and sorted_votes =
      List.map
        (fun x ->
          let d = v.data x
          and hash = v.pow_hash x |> Option.value ~default:(0, 0) in
          d.vote, hash)
        votes
      |> is_sorted ~unique:true Compare.(tuple (neg int) (tuple int int))
    in
    is_block parent
    && sorted_votes
    && List.for_all (fun n -> v.data n |> is_vote) votes
    && unique_votes = k - 1
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

type ('env, 'dag_data) extended_view =
  { view : 'env Dag.view
  ; data : 'env Dag.vertex -> 'dag_data
  ; votes_only : 'env Dag.view
  ; blocks_only : 'env Dag.view
  ; delivered_at : 'env Dag.vertex -> float
  ; pow_hash : 'env Dag.vertex -> (int * int) option
  ; appended_by_me : 'env Dag.vertex -> bool
  ; released : 'env Dag.vertex -> bool
  ; my_id : int
  }

let max_pow_hash = max_int, max_int

let extend_view (x : _ local_view) =
  { view = x.view
  ; data = x.data
  ; votes_only = Dag.filter (fun n -> x.data n |> is_vote) x.view
  ; blocks_only = Dag.filter (fun n -> x.data n |> is_block) x.view
  ; delivered_at = x.delivered_at
  ; pow_hash = x.pow_hash
  ; appended_by_me = x.appended_by_me
  ; released = x.released
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

module IntSet = Set.Make (struct
  type t = int

  let compare = compare
end)

let quorum ~k v for_block =
  let rec f ids n q = function
    | [] -> None
    | hd :: tl ->
      let fresh, n_fresh =
        Dag.iterate_ancestors v.votes_only [ hd ]
        |> Seq.fold_left
             (fun (fresh, n_fresh) el ->
               let id = Dag.id el in
               if IntSet.mem id ids
               then fresh, n_fresh
               else IntSet.add id fresh, n_fresh + 1)
             (IntSet.empty, 0)
      in
      if n_fresh + n > k - 1 || n_fresh < 1
      then f ids n q tl
      else if n_fresh + n = k - 1
      then Some (List.rev (hd :: q))
      else f (IntSet.union fresh ids) (n + n_fresh) (hd :: q) tl
  in
  Dag.iterate_descendants v.votes_only [ for_block ]
  |> Seq.filter (Dag.vertex_neq for_block)
  |> List.of_seq
  |> List.sort
       Compare.(
         by
           (tuple (neg int) (tuple int float))
           (fun x ->
             (v.data x).vote, ((if v.appended_by_me x then 0 else 1), v.delivered_at x)))
  |> f IntSet.empty 0 []
  |> Option.map
       (List.sort
          Compare.(
            by
              (tuple (neg int) (tuple int int))
              (fun x ->
                let d = v.data x
                and hash = v.pow_hash x |> Option.value ~default:(0, 0) in
                d.vote, hash)))
;;

let honest ~k v =
  let v = extend_view v in
  let handler actions preferred = function
    | Activate pow ->
      let head = last_block v preferred in
      (match quorum ~k v head with
      | Some q ->
        let head' =
          actions.extend_dag
            ~pow
            (head :: q)
            { block = (v.data head).block + 1; vote = 0 }
        in
        let () = actions.share head' in
        head'
      | None ->
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
  { init; handler; preferred = (fun x -> x) }
;;

let protocol ~k =
  { honest = honest ~k; dag_validity = dag_validity ~k; dag_roots; describe }
;;

let%test "convergence" =
  let open Simulator in
  let test k params height =
    let env = all_honest params (protocol ~k) |> init in
    loop params env;
    Array.to_seq env.nodes
    |> Seq.map (fun (Node x) -> x.preferred x.state)
    |> Dag.common_ancestor'
         (Dag.filter (fun x -> is_block (Dag.data x).value) env.global.view)
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

let reward ~max_reward_per_block ~discount ~punish ~k : ('env, dag_data) reward_function =
  let k = float_of_int k in
  let c = max_reward_per_block /. k in
  fun ~view:v ~assign ->
    let votes_only = Dag.filter (fun x -> v.data x |> is_vote) v.view in
    fun n ->
      if v.data n |> is_block
      then (
        match Dag.parents votes_only n with
        | [] -> (* Either genesis or k=1 *) assign c n
        | hd :: _ ->
          let depth = (v.data hd).vote in
          let x = if discount then (float_of_int depth +. 1.) /. k *. c else c in
          if punish
          then (
            assign x n;
            Dag.iterate_ancestors votes_only [ hd ] |> Seq.iter (assign x))
          else Dag.iterate_ancestors votes_only [ n ] |> Seq.iter (assign x))
;;

(* TODO: add tests for reward functions *)
