type 'a iid = unit -> 'a

let constant a () = a
let uniform ~lower ~upper () = Random.float (upper -. lower) +. lower
let exponential ~ev () = -1. *. ev *. log (Random.float 1.)

(* Voses Alias Method for efficient sampling of discrete random variables
   http://keithschwarz.com/darts-dice-coins/
   https://alaska-kamtchatka.blogspot.com/2011/12/voses-alias-method.html

   TODO: Manual tests show that it works. Proper evaluation is missing! *)
let discrete ~weights =
  (* collect info about probabilities *)
  let sum, n =
    List.fold_left
      (fun (sum, n) p ->
        if p < 0.
        then raise (Invalid_argument "negative probability")
        else sum +. p, n + 1)
      (0., 0)
      weights
  in
  if n < 1 then raise (Invalid_argument "empty list");
  (* prepare alias table *)
  let p = Array.make n 0.
  and alias = Array.make n None
  and small, large, _ =
    let scale = float_of_int n /. sum in
    List.fold_left
      (fun (s, l, i) p ->
        let p = p *. scale in
        if p < 1. then (p, i) :: s, l, i + 1 else s, (p, i) :: l, i + 1)
      ([], [], 0)
      weights
  in
  let () =
    (* init alias table *)
    let rec f = function
      | (ps, is) :: small, (pl, il) :: large ->
        p.(is) <- ps;
        alias.(is) <- Some il;
        let p' = ps +. pl -. 1. in
        if p' < 1. then f ((p', il) :: small, large) else f (small, (p', il) :: large)
      | (_p, i) :: small, [] ->
        p.(i) <- 1.;
        alias.(i) <- None;
        f (small, [])
      | [], (_p, i) :: large ->
        p.(i) <- 1.;
        alias.(i) <- None;
        f ([], large)
      | [], [] -> ()
    in
    f (small, large)
  in
  (* sample *)
  fun () ->
    let i = Random.int n in
    match alias.(i) with
    | None -> i
    | Some j -> if Random.float 1. > p.(i) then j else i
;;
