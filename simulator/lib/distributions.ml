open Rresult

type 'a iid =
  { sample : unit -> 'a
  ; string : string lazy_t
  }

let sample t = t.sample ()
let to_string t = Lazy.force t.string
let pp fmt t = Format.pp_print_string fmt (to_string t)

let constant x =
  { sample = (fun () -> x); string = lazy (Printf.sprintf "constant %g" x) }
;;

let uniform ~lower ~upper =
  let sample () = Random.float (upper -. lower) +. lower
  and string = lazy (Printf.sprintf "uniform %g %g" lower upper) in
  { sample; string }
;;

let exponential ~ev =
  let sample () =
    let x = -1. *. ev *. log (Random.float 1.) in
    assert (x > 0.);
    x
  in
  { sample; string = lazy (Printf.sprintf "exponential %g" ev) }
;;

let geometric ~success_probability =
  let sample () =
    let x = log (Random.float 1.) /. log (1. -. success_probability) in
    let x = floor x |> int_of_float in
    x
  in
  { sample; string = lazy (Printf.sprintf "geometric %g" success_probability) }
;;

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
  let sample () =
    let i = Random.int n in
    match alias.(i) with
    | None -> i
    | Some j -> if Random.float 1. > p.(i) then j else i
  and string =
    lazy (String.concat " " ("discrete" :: List.map string_of_float weights))
  in
  { sample; string }
;;

module Parsers = struct
  open Angstrom

  let whitespace = function
    | ' ' -> true
    | _ -> false
  ;;

  let space = skip_many1 (skip whitespace)

  let float_literal =
    let* str = take_till whitespace in
    match float_of_string_opt str with
    | Some x -> return x
    | None -> fail "could not parse float"
  ;;

  let constant = string "constant" *> space *> lift constant float_literal

  let uniform =
    string "uniform"
    *> space
    *> lift2
         (fun lower upper -> uniform ~lower ~upper)
         float_literal
         (space *> float_literal)
  ;;

  let exponential =
    string "exponential" *> space *> lift (fun ev -> exponential ~ev) float_literal
  ;;

  let trim = skip_many (skip whitespace)

  let float =
    trim *> constant <|> uniform <|> exponential <|> fail "unknown distribution" <* trim
  ;;
end

let float_of_string s =
  Angstrom.parse_string ~consume:Angstrom.Consume.All Parsers.float s
  |> R.reword_error R.msg
;;

let float_of_string_memoize () =
  let t = Hashtbl.create 42 in
  fun s ->
    match Hashtbl.find_opt t s with
    | Some x -> x
    | None ->
      let x = float_of_string s in
      Hashtbl.add t s x;
      x
;;

let%test_module "float" =
  (module struct
    let expect_ok s =
      match float_of_string s with
      | Error (`Msg e) ->
        Printf.eprintf "'%s' yields error '%s'" s e;
        false
      | _ -> true
    ;;

    let expect_bad s =
      match float_of_string s with
      | Ok _ ->
        Printf.printf "'%s' should not parse but does" s;
        false
      | _ -> true
    ;;

    let%test _ = expect_bad ""
    let%test _ = expect_bad "random"
    let%test _ = expect_ok "constant 1"
    let%test _ = expect_ok "constant 0"
    let%test _ = expect_ok "constant 1.2"
    let%test _ = expect_bad "constant"
    let%test _ = expect_ok "uniform 1.2 2"
    let%test _ = expect_bad "uniform 1"
    let%test _ = expect_ok "exponential 1.2"
    let%test _ = expect_bad "exponential 1 2"
  end)
;;
