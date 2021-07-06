open Cpr_lib
open Protocol

type height =
  { block : int
  ; vote : int
  }

let is_vote h = h.vote > 0
let is_block h = h.vote = 0

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

let spawn ~k ctx =
  let votes_only = Dag.filter (fun n -> ctx.read n |> is_vote) ctx.view
  and blocks_only = Dag.filter (fun n -> ctx.read n |> is_block) ctx.view
  and data n = Dag.data n |> ctx.read in
  let handler (lb, pv) (* last block, preferred chain *) = function
    | Activate pow ->
      let votes = offspring votes_only lb in
      if List.length votes >= k - 1
      then (
        let lb' = data lb in
        let b =
          ctx.extend_dag
            ~pow
            (lb :: first (k - 1) votes) (* TODO first should be first received *)
            { block = lb'.block + 1; vote = 0 }
        in
        let () = ctx.share b in
        b, b)
      else (
        let pv' = data pv in
        let v = ctx.extend_dag ~pow [ pv ] { pv' with vote = pv'.vote + 1 } in
        let () = ctx.share v in
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
  { init; handler }
;;

let protocol ~k =
  { spawn = spawn ~k; dag_invariant = dag_invariant ~k; dag_roots; head = fst }
;;

let%test "convergence" =
  let open Simulator in
  let test k params height =
    let protocol = protocol ~k in
    init params protocol
    |> loop params
    |> fun { nodes; global_view; _ } ->
    Array.to_seq nodes
    |> Seq.map (fun x -> protocol.head x.state)
    |> Dag.common_ancestor' (Dag.filter (fun x -> is_block x.value) global_view)
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

let constant_reward_per_pow ?(reward_per_block = 1.) ~k : ('env, height) reward_function =
  let c = reward_per_block /. float_of_int k in
  fun view read miner head arr ->
    let blocks = Dag.filter (fun x -> read x |> is_block) view
    and votes = Dag.filter (fun x -> read x |> is_vote) view
    and reward gb =
      match miner (Dag.data gb) with
      | None -> ()
      | Some miner -> arr.(miner) +. c |> Array.set arr miner
    in
    Seq.iter
      (fun b ->
        reward b;
        List.iter reward (Dag.parents votes b))
      (Dag.seq_history blocks head)
;;

let discount_vote_depth ?(max_reward_per_block = 1.) ~k : ('env, height) reward_function =
  let k = float_of_int k in
  let c = max_reward_per_block /. k /. k in
  fun view read miner head arr ->
    let blocks = Dag.filter (fun x -> read x |> is_block) view
    and votes = Dag.filter (fun x -> read x |> is_vote) view
    and reward x gb =
      match miner (Dag.data gb) with
      | None -> ()
      | Some miner -> arr.(miner) +. x |> Array.set arr miner
    in
    Seq.iter
      (fun b ->
        let votes = Dag.parents votes b in
        let depth =
          List.fold_left (fun acc v -> max acc (Dag.data v |> read).vote) 0 votes
          |> float_of_int
        in
        let x = (depth +. 1.) *. c in
        reward x b;
        List.iter (reward x) votes)
      (Dag.seq_history blocks head)
;;

let punish_nonlinear ?(max_reward_per_block = 1.) ~k : ('env, height) reward_function =
  let c = max_reward_per_block /. float_of_int k in
  fun view read miner head arr ->
    let blocks = Dag.filter (fun x -> read x |> is_block) view
    and votes = Dag.filter (fun x -> read x |> is_vote) view
    and reward x gb =
      match miner (Dag.data gb) with
      | None -> ()
      | Some miner -> arr.(miner) +. x |> Array.set arr miner
    in
    Seq.iter
      (fun b ->
        reward c b;
        match Dag.parents votes b with
        | [] -> ()
        | hd :: tl ->
          let get_vdepth v = (Dag.data v |> read).vote in
          let longest, _ =
            List.fold_left
              (fun acc v ->
                let depth = get_vdepth v in
                if depth > snd acc then v, depth else acc)
              (hd, get_vdepth hd)
              tl
          in
          Dag.seq_history votes longest |> Seq.iter (reward c))
      (Dag.seq_history blocks head)
;;

(* TODO: add tests for reward functions *)
