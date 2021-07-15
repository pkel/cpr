open Cpr_lib
open Protocol

type height =
  { block : int
  ; vote : int
  }

let is_vote h = h.vote > 0
let is_block h = h.vote = 0

(* TODO BUG: more than k pows might be referenced. Check nested references. *)
let dag_invariant ~k ~pow parents child =
  child.block >= 0
  && child.vote >= 0
  && child.vote < k
  && pow
  &&
  match is_vote child, parents with
  | true, [ parent ] ->
    (* child is vote *)
    child.vote = parent.vote + 1
  | false, parent :: votes ->
    (* child is block *)
    is_block parent
    && List.for_all is_vote votes
    && List.length votes = k - 1
    && child.block = parent.block + 1
    && child.vote = 0
  | _ -> false
;;

let dag_roots = [ { block = 0; vote = 0 } ]

let init ~roots =
  match roots with
  | [ genesis ] -> genesis, genesis
  | _ -> failwith "invalid roots"
;;

let rec last_block ctx gnode =
  let node = Dag.data gnode |> ctx.read in
  if is_block node
  then Some gnode
  else (
    match Dag.parents ctx.view gnode with
    | [] -> None (* gnode not connected yet / visibility *)
    | [ gnode ] -> last_block ctx gnode
    | _ -> failwith "invalid dag")
;;

(* votes have only one parent by dag_invariant *)

let first n =
  let rec h n acc l =
    if n <= 0
    then List.rev acc
    else (
      match l with
      | [] -> raise (Invalid_argument "list too short")
      | hd :: tl -> h (n - 1) (hd :: acc) tl)
  in
  h n []
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

let honest ~k ctx =
  let data n = Dag.data n |> ctx.read in
  let votes_only = Dag.filter (fun n -> data n |> is_vote) ctx.view
  and blocks_only = Dag.filter (fun n -> data n |> is_block) ctx.view in
  let handler actions (lb, pv) (* last block, preferred chain *) = function
    | Activate pow ->
      let votes = offspring votes_only lb in
      if List.length votes >= k - 1
      then (
        let lb' = data lb in
        let b =
          actions.extend_dag
            ~pow
            (* TODO Bug: referencing the last k received leads to more than
             * k proof-of-work per block *)
            (lb :: first (k - 1) votes)
            (* TODO first should be first received *)
            { block = lb'.block + 1; vote = 0 }
        in
        let () = actions.share b in
        b, b)
      else (
        let pv' = data pv in
        let v = actions.extend_dag ~pow [ pv ] { pv' with vote = pv'.vote + 1 } in
        let () = actions.share v in
        lb, v)
    | Deliver gnode ->
      (* Prefer longest chain of votes after longest chain of blocks *)
      (match last_block ctx gnode with
      | None -> lb, pv (* vote not yet connected to last block *)
      | Some gblock ->
        (* Only consider block if its heritage is visible. *)
        if Dag.have_common_ancestor blocks_only gblock lb
        then (
          let consider gpref gcand =
            let pref = data gpref
            and cand = data gcand in
            if cand.block > pref.block
               || (cand.block = pref.block && cand.vote > pref.vote)
            then gcand
            else gpref
          in
          (* delayed nodes might connect nodes delivered previously *)
          let pv = List.fold_left consider pv (Dag.leaves ctx.view gblock) in
          last_block ctx pv |> Option.get, pv)
        else lb, pv)
  in
  Node { init; handler; preferred = fst }
;;

let protocol ~k = { honest = honest ~k; dag_invariant = dag_invariant ~k; dag_roots }

let%test "convergence" =
  let open Simulator in
  let test k params height =
    let protocol = protocol ~k in
    init params protocol
    |> loop params
    |> fun { nodes; global_view; _ } ->
    Array.to_seq nodes
    |> Seq.map (fun (SNode x) -> x.preferred x.state)
    |> Dag.common_ancestor'
         (Dag.filter (fun x -> is_block (Dag.data x).value) global_view)
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

let reward ~max_reward_per_block ~discount ~punish ~k : ('env, height) reward_function =
  let k = float_of_int k in
  let c = max_reward_per_block /. k in
  fun ~view ~read ~assign ->
    let data n = Dag.data n |> read in
    let vote_view = Dag.filter (fun x -> data x |> is_vote) view in
    fun n ->
      if data n |> is_block
      then (
        match Dag.parents vote_view n with
        | [] -> (* Either genesis or k=1 *) assign c n
        | hd :: tl as votes ->
          let get_vdepth v = (Dag.data v |> read).vote in
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
