type value =
  | Bool of bool
  | Float of float
  | Int of int
  | String of string

let string_of_value = function
  | Bool x -> string_of_bool x
  | Float x -> string_of_float x
  | Int x -> string_of_int x
  | String x -> x
;;

let bool k x = k, Bool x
let float k x = k, Float x
let int k x = k, Int x
let string k x = k, String x

type t = (string * value) list

let map_key f = List.map (fun (k, v) -> f k, v)
let prefix_key s = map_key (fun k -> s ^ k)

let pp_rows ~sep fmt rows =
  let cols, colidx, ncols =
    let ht = Hashtbl.create 42
    and i = ref 0 in
    List.iter
      (fun info ->
        List.iter
          (fun (k, _v) ->
            if not (Hashtbl.mem ht k)
            then (
              Hashtbl.replace ht k !i;
              incr i))
          info)
      rows;
    let cols = Array.make !i "" in
    Hashtbl.iter (fun k v -> cols.(v) <- k) ht;
    cols, Hashtbl.find ht, !i
  in
  let row a =
    let open Format in
    Array.iteri
      (fun i s ->
        pp_print_string fmt s;
        if i < ncols - 1 then pp_print_string fmt sep)
      a;
    pp_print_newline fmt ()
  in
  row cols;
  List.iter
    (fun t ->
      let a = Array.make ncols "" in
      List.iter (fun (k, v) -> a.(colidx k) <- string_of_value v) t;
      row a)
    rows
;;

let%expect_test "pp_rows" =
  let rows =
    [ [ "a", Int 1; "b", Float 2. ]
    ; [ "a", Int 7; "b", Float 42. ]
    ; [ "a", Int 1; "c", Float 5. ]
    ]
  in
  pp_rows ~sep:"\t" Format.std_formatter rows;
  [%expect {|
    a	b	c
    1	2.
    7	42.
    1		5. |}]
;;
