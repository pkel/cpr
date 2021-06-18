(* ordered queue
 * invariant: top element is next in queue
 *)
type ('a, 'b) tree =
  | Empty
  | Node of 'a * 'b * ('a, 'b) tree * ('a, 'b) tree

type ('a, 'b) t =
  { tree: ('a, 'b) tree
  ; compare: 'a -> 'a -> int
  ; len: int
  }

let init compare =
  { compare
  ; tree = Empty
  ; len = 0
  }

let length t = t.len

let queue time evt t =
  let rec h time evt = function
    | Empty -> Node (time, evt, Empty, Empty)
    | Node (this_time, this_evt, left, right) ->
      if t.compare time this_time < 0 then Node (time, evt, h this_time this_evt left, right)
      else Node (this_time, this_evt, h time evt right, left)
  in
  { t with tree = h time evt t.tree
         ; len = t.len + 1
  }

let rec remove_top compare = function
  | Empty -> raise Not_found
  | Node (_time, _evt, left, Empty) -> left
  | Node (_time, _evt, Empty, right) -> right
  | Node
      ( _time
      , _evt
      , (Node (ltime, levt, _, _) as left)
      , (Node (rtime, revt, _, _) as right) ) ->
      if compare ltime rtime <= 0 then Node (ltime, levt, remove_top compare left, right)
      else Node (rtime, revt, left, remove_top compare right)

let dequeue t =
  match t.tree with
  | Empty -> None
  | Node (time, evt, _, _) ->
    Some (time, evt, {t with tree = remove_top t.compare t.tree
                           ; len = t.len - 1
                     })

let%test _ =
  let queue =
    List.init 100 (fun i -> i)
    |> List.map (fun _i -> Random.float 42.)
    |> List.fold_left (fun q t -> queue t () q) (init Float.compare)
  and time = ref Float.neg_infinity
  in
  let rec test queue =
    match dequeue queue with
    | None -> true
    | Some (t, (), queue) ->
      if t < !time then false
      else (
        time := t;
        test queue
      )
  in test queue

let%test _ =
  let time = ref 0 in
  let queue n q =
    List.init n (fun i -> i)
    |> List.map (fun _i -> !time + Random.int 42)
    |> List.fold_left (fun q t -> queue t () q) q
  in
  let rec consume n q =
    if n > 0 then
      match dequeue q with
      | None -> raise Not_found
      | Some (t, (), queue) ->
        if t < !time then failwith "invalid order"
        else (
          time := t;
          consume (n - 1) queue
        )
    else q
  in
  try
    let _ =
      List.fold_left
        (fun q _ -> queue 50 q |> consume 42)
        (init Int.compare)
        (List.init 100 (fun i -> i))
    in
    true
  with _ -> false
