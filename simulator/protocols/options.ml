open Cpr_lib

let to_string = function
  | `Altruistic -> "altruistic"
  | `Block -> "block"
  | `Constant -> "constant"
  | `Discount -> "discount"
  | `HeaviestChain -> "heaviest_chain"
  | `Height -> "height"
  | `Heuristic -> "heuristic"
  | `Hybrid -> "hybrid"
  | `Infinity -> "infinity"
  | `Int i -> string_of_int i
  | `LongestChain -> "longest_chain"
  | `Optimal -> "optimal"
  | `Punish -> "punish"
  | `Work -> "work"
;;

let pp fmt o = to_string o |> Format.pp_print_string fmt
let info k v = Info.string k (to_string v)

let pp_numeration ?quote ?(sep = ", ") ?(conj = " and ") pp =
  let open Format in
  let pp =
    match quote with
    | Some s ->
      fun fmt x ->
        pp_print_string fmt s;
        pp fmt x;
        pp_print_string fmt s
    | None -> pp
  in
  fun fmt ->
    let rec f = function
      | [] -> assert false
      | [ hd ] ->
        pp_print_string fmt sep;
        pp_print_string fmt conj;
        pp fmt hd
      | hd :: tl ->
        pp_print_string fmt sep;
        pp fmt hd;
        f tl
    in
    function
    | [] -> pp_print_string fmt "<n/a>"
    | [ a ] -> pp fmt a
    | [ a; b ] ->
      pp fmt a;
      pp_print_string fmt conj;
      pp fmt b
    | x -> f x
;;

let of_string choice =
  let table = Hashtbl.create (List.length choice) in
  List.iter (fun x -> Hashtbl.replace table (to_string x) x) choice;
  fun string ->
    match Hashtbl.find_opt table string with
    | Some x -> Ok x
    | None ->
      let msg =
        Format.asprintf
          "'%s' is not a valid parameter choice, try %a"
          string
          (pp_numeration ~quote:"'" ~conj:" or " pp)
          choice
      in
      Error msg
;;

let of_string_exn choice string =
  match of_string choice string with
  | Ok x -> x
  | Error msg -> raise (Invalid_argument msg)
;;
