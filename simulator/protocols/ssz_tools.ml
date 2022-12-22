module NormalizeObs = struct
  type 'a field =
    | Bool : bool field
    | Discrete : 'a list -> 'a field
    | UnboundedInt :
        { scale : int
        ; non_negative : bool
        }
        -> int field

  let to_float_raw (type a) (t : a field) : a -> float =
    match t with
    | Bool -> fun b -> if b then 1. else 0.
    | Discrete l ->
      let l = List.mapi (fun i x -> x, float_of_int i) l in
      fun x -> List.assoc x l
    | UnboundedInt _ -> fun x -> float_of_int x
  ;;

  let of_float_raw (type a) (t : a field) : float -> a =
    match t with
    | Bool -> fun x -> x >= 0.5
    | Discrete l ->
      let arr = Array.of_list l in
      fun x -> arr.(int_of_float x)
    | UnboundedInt _ -> fun x -> int_of_float x
  ;;

  let to_float_unit (type a) (t : a field) : a -> float =
    match t with
    | Bool -> fun b -> if b then 1. else 0.
    | Discrete l ->
      let max = List.length l - 1 |> float_of_int in
      let l = List.mapi (fun i x -> x, float_of_int i /. max) l in
      fun x -> List.assoc x l
    | UnboundedInt { non_negative = true; scale } ->
      fun x -> 2. /. Float.pi *. Float.atan (float_of_int x /. float_of_int scale)
    | UnboundedInt { non_negative = false; scale } ->
      fun x -> 0.5 +. (1. /. Float.pi *. Float.atan (float_of_int x /. float_of_int scale))
  ;;

  let of_float_unit (type a) (t : a field) : float -> a =
    match t with
    | Bool -> fun x -> x >= 0.5
    | Discrete l ->
      let max = List.length l - 1 |> float_of_int
      and arr = Array.of_list l in
      fun x -> arr.(Float.floor (x *. max) |> int_of_float)
    | UnboundedInt { non_negative = true; scale } ->
      fun x ->
        Float.tan (Float.pi /. 2. *. x) *. float_of_int scale
        |> Float.round
        |> Float.to_int
    | UnboundedInt { non_negative = false; scale } ->
      fun x ->
        Float.tan (Float.pi *. (x -. 0.5)) *. float_of_int scale
        |> Float.round
        |> Float.to_int
  ;;

  let of_float ~unit = if unit then of_float_unit else of_float_raw
  let to_float ~unit = if unit then to_float_unit else to_float_raw

  let range (type a) (t : a field) : float * float =
    match t with
    | Bool -> 0., 0.
    | Discrete l -> 0., List.length l - 1 |> float_of_int
    | UnboundedInt { non_negative = true; scale = _ } -> 0., Float.infinity
    | UnboundedInt { non_negative = false; scale = _ } ->
      Float.neg_infinity, Float.infinity
  ;;

  let range ~unit t = if unit then 0., 1. else range t
end

let event_to_string = function
  | `Append -> "`Append"
  | `ProofOfWork -> "`ProofOfWork"
  | `Network -> "`Network"
;;

let%test_module "normalize unit" =
  (module struct
    open NormalizeObs

    let to_float x = to_float ~unit:true x
    let of_float x = of_float ~unit:true x
    let t = Bool

    let%test "bool" = to_float t true |> of_float t = true
    let%test "bool" = to_float t false |> of_float t = false

    let t = Discrete [ `A; `B; `C ]

    let%test "discrete" = to_float t `A |> of_float t = `A
    let%test "discrete" = to_float t `B |> of_float t = `B
    let%test "discrete" = to_float t `C |> of_float t = `C
    let%test "discrete" = to_float t `A = 0.
    let%test "discrete" = to_float t `C = 1.

    let t = UnboundedInt { non_negative = true; scale = 1 }

    let%test "unbounded int non_neg" = to_float t 0 |> of_float t = 0
    let%test "unbounded int non_neg" = to_float t 1 |> of_float t = 1
    let%test "unbounded int non_neg" = to_float t 2 |> of_float t = 2
    let%test "unbounded int non_neg" = to_float t 256 |> of_float t = 256
    let%test "unbounded int non_neg" = to_float t 0 = 0.
    let%test "unbounded int non_neg" = to_float t 1 = 0.5
    let%test "unbounded int non_neg" = to_float t max_int = 1.

    let t = UnboundedInt { non_negative = false; scale = 1 }

    let%test "unbounded int" = to_float t 0 |> of_float t = 0
    let%test "unbounded int" = to_float t 1 |> of_float t = 1
    let%test "unbounded int" = to_float t 2 |> of_float t = 2
    let%test "unbounded int" = to_float t 256 |> of_float t = 256
    let%test "unbounded int" = to_float t (-1) |> of_float t = -1
    let%test "unbounded int" = to_float t (-2) |> of_float t = -2
    let%test "unbounded int" = to_float t (-256) |> of_float t = -256
    let%test "unbounded int" = to_float t min_int = 0.
    let%test "unbounded int" = to_float t (-1) = 0.25
    let%test "unbounded int" = to_float t 0 = 0.5
    let%test "unbounded int" = to_float t 1 = 0.75
    let%test "unbounded int" = to_float t max_int = 1.

    let t = UnboundedInt { non_negative = true; scale = 4 }

    let%test "unbounded int non_neg scale 4" = to_float t 0 |> of_float t = 0
    let%test "unbounded int non_neg scale 4" = to_float t 1 |> of_float t = 1
    let%test "unbounded int non_neg scale 4" = to_float t 2 |> of_float t = 2
    let%test "unbounded int non_neg scale 4" = to_float t 256 |> of_float t = 256
    let%test "unbounded int non_neg scale 4" = to_float t (-1) |> of_float t = -1
    let%test "unbounded int non_neg scale 4" = to_float t (-2) |> of_float t = -2
    let%test "unbounded int non_neg scale 4" = to_float t (-256) |> of_float t = -256
    let%test "unbounded int non_neg scale 4" = to_float t 0 = 0.
    let%test "unbounded int non_neg scale 4" = to_float t 4 = 0.5
    let%test "unbounded int non_neg scale 4" = to_float t max_int = 1.

    let t = UnboundedInt { non_negative = false; scale = 4 }

    let%test "unbounded int scale 4" = to_float t 0 |> of_float t = 0
    let%test "unbounded int scale 4" = to_float t 1 |> of_float t = 1
    let%test "unbounded int scale 4" = to_float t 2 |> of_float t = 2
    let%test "unbounded int scale 4" = to_float t 256 |> of_float t = 256
    let%test "unbounded int scale 4" = to_float t (-1) |> of_float t = -1
    let%test "unbounded int scale 4" = to_float t (-2) |> of_float t = -2
    let%test "unbounded int scale 4" = to_float t (-256) |> of_float t = -256
    let%test "unbounded int scale 4" = to_float t min_int = 0.
    let%test "unbounded int scale 4" = to_float t (-4) = 0.25
    let%test "unbounded int scale 4" = to_float t 0 = 0.5
    let%test "unbounded int scale 4" = to_float t 4 = 0.75
    let%test "unbounded int scale 4" = to_float t max_int = 1.
  end)
;;

let%test_module "normalize raw" =
  (module struct
    open NormalizeObs

    let to_float x = to_float ~unit:false x
    let of_float x = of_float ~unit:false x
    let t = Bool

    let%test "bool" = to_float t true |> of_float t = true
    let%test "bool" = to_float t false |> of_float t = false

    let t = Discrete [ `A; `B; `C ]

    let%test "discrete" = to_float t `A |> of_float t = `A
    let%test "discrete" = to_float t `B |> of_float t = `B
    let%test "discrete" = to_float t `C |> of_float t = `C
    let%test "discrete" = to_float t `A = 0.
    let%test "discrete" = to_float t `C = 2.

    let t = UnboundedInt { non_negative = true; scale = 1 }

    let%test "unbounded int non_neg" = to_float t 0 |> of_float t = 0
    let%test "unbounded int non_neg" = to_float t 1 |> of_float t = 1
    let%test "unbounded int non_neg" = to_float t 2 |> of_float t = 2
    let%test "unbounded int non_neg" = to_float t 256 |> of_float t = 256
    let%test "unbounded int non_neg" = to_float t 0 = 0.
    let%test "unbounded int non_neg" = to_float t 1 = 1.
    let%test "unbounded int non_neg" = to_float t 42 = 42.

    let t = UnboundedInt { non_negative = false; scale = 1 }

    let%test "unbounded int" = to_float t 0 |> of_float t = 0
    let%test "unbounded int" = to_float t 1 |> of_float t = 1
    let%test "unbounded int" = to_float t 2 |> of_float t = 2
    let%test "unbounded int" = to_float t 256 |> of_float t = 256
    let%test "unbounded int" = to_float t (-1) |> of_float t = -1
    let%test "unbounded int" = to_float t (-2) |> of_float t = -2
    let%test "unbounded int" = to_float t (-256) |> of_float t = -256
    let%test "unbounded int" = to_float t (-42) = -42.
    let%test "unbounded int" = to_float t (-1) = -1.
    let%test "unbounded int" = to_float t 0 = 0.
    let%test "unbounded int" = to_float t 1 = 1.
    let%test "unbounded int" = to_float t 42 = 42.

    let t = UnboundedInt { non_negative = true; scale = 4 }

    let%test "unbounded int non_neg scale 4" = to_float t 0 |> of_float t = 0
    let%test "unbounded int non_neg scale 4" = to_float t 1 |> of_float t = 1
    let%test "unbounded int non_neg scale 4" = to_float t 2 |> of_float t = 2
    let%test "unbounded int non_neg scale 4" = to_float t 256 |> of_float t = 256
    let%test "unbounded int non_neg scale 4" = to_float t (-1) |> of_float t = -1
    let%test "unbounded int non_neg scale 4" = to_float t (-2) |> of_float t = -2
    let%test "unbounded int non_neg scale 4" = to_float t (-256) |> of_float t = -256
    let%test "unbounded int non_neg scale 4" = to_float t 0 = 0.
    let%test "unbounded int non_neg scale 4" = to_float t 4 = 4.
    let%test "unbounded int non_neg scale 4" = to_float t 42 = 42.

    let t = UnboundedInt { non_negative = false; scale = 4 }

    let%test "unbounded int scale 4" = to_float t 0 |> of_float t = 0
    let%test "unbounded int scale 4" = to_float t 1 |> of_float t = 1
    let%test "unbounded int scale 4" = to_float t 2 |> of_float t = 2
    let%test "unbounded int scale 4" = to_float t 256 |> of_float t = 256
    let%test "unbounded int scale 4" = to_float t (-1) |> of_float t = -1
    let%test "unbounded int scale 4" = to_float t (-2) |> of_float t = -2
    let%test "unbounded int scale 4" = to_float t (-256) |> of_float t = -256
    let%test "unbounded int scale 4" = to_float t (-42) = -42.
    let%test "unbounded int scale 4" = to_float t (-4) = -4.
    let%test "unbounded int scale 4" = to_float t 0 = 0.
    let%test "unbounded int scale 4" = to_float t 4 = 4.
    let%test "unbounded int scale 4" = to_float t 42 = 42.
  end)
;;

module Action8 = struct
  type t =
    | Adopt_Prolong
    | Override_Prolong
    | Match_Prolong
    | Wait_Prolong
    | Adopt_Proceed
    | Override_Proceed
    | Match_Proceed
    | Wait_Proceed
  [@@deriving variants]

  let to_string = Variants.to_name
  let to_int = Variants.to_rank

  let table =
    let add acc var = var.Variantslib.Variant.constructor :: acc in
    Variants.fold
      ~init:[]
      ~adopt_prolong:add
      ~override_prolong:add
      ~match_prolong:add
      ~wait_prolong:add
      ~adopt_proceed:add
      ~override_proceed:add
      ~match_proceed:add
      ~wait_proceed:add
    |> List.rev
    |> Array.of_list
  ;;

  let of_int i = table.(i)
  let n = Array.length table
end
