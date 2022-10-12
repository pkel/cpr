open Cpr_lib

module Event = struct
  let events = [ `Append; `Network; `ProofOfWork ]

  let to_int = function
    | `Append -> 0
    | `Network -> 1
    | `ProofOfWork -> 2
  ;;

  let of_int =
    let a = Array.of_list events in
    fun i -> a.(i)
  ;;

  let low = List.map to_int events |> List.fold_left min max_int
  let high = List.map to_int events |> List.fold_left max min_int
end

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

module State8 (V : LocalView) : sig
  open V

  type t = private
    { public : env Dag.vertex (* defender's preferred block *)
    ; private_ : env Dag.vertex (* attacker's preferred block *)
    ; common : env Dag.vertex (* common chain *)
    ; epoch : [ `Proceed | `Prolong ]
          (* Proceed: the attacker considers the defender's votes that extend on his
             preferred block when building a new block.

             Prolong: the attacker prolongs the current epoch until he can form a block
             that does not reference any defender votes. *)
    ; pending_private_to_public_messages : env Dag.vertex list
    }

  val init : epoch:[ `Proceed | `Prolong ] -> env Dag.vertex -> t

  (* Set fields in state; updates common chain *)
  val update
    :  ?public:env Dag.vertex
    -> ?private_:env Dag.vertex
    -> ?epoch:[ `Proceed | `Prolong ]
    -> ?pending_private_to_public_messages:env Dag.vertex list
    -> t
    -> t
end = struct
  open V

  type t =
    { public : env Dag.vertex
    ; private_ : env Dag.vertex
    ; common : env Dag.vertex
    ; epoch : [ `Proceed | `Prolong ]
    ; pending_private_to_public_messages : env Dag.vertex list
    }

  let init ~epoch x =
    { public = x
    ; private_ = x
    ; common = x
    ; epoch
    ; pending_private_to_public_messages = []
    }
  ;;

  (* call this whenever public or private_ changes *)
  let set_common state =
    let common = Dag.common_ancestor view state.public state.private_ in
    assert (Option.is_some common) (* all our protocols maintain this invariant *);
    { state with common = Option.get common }
  ;;

  let update ?public ?private_ ?epoch ?pending_private_to_public_messages t =
    set_common
      { public = Option.value ~default:t.public public
      ; private_ = Option.value ~default:t.private_ private_
      ; epoch = Option.value ~default:t.epoch epoch
      ; common = t.common
      ; pending_private_to_public_messages =
          Option.value
            ~default:t.pending_private_to_public_messages
            pending_private_to_public_messages
      }
  ;;
end

let emulated_view
    (type a b)
    ~pretend_not_me
    ~filter
    (module V : LocalView with type data = a and type env = b)
  =
  (module struct
    type env = V.env
    type data = V.data

    let view = Dag.filter filter V.view

    let guard f x =
      if filter x then f x else failwith "visibility violation in emulated_view"
    ;;

    let data = guard V.data
    let visibility x = if pretend_not_me then `Received else V.visibility x
    let visibility = guard visibility
    let visible_since = guard V.visible_since
    let pow = guard V.pow
    let signature = guard V.signature
    let my_id = if pretend_not_me then -1 else V.my_id
    let min_pow = V.min_pow
    let max_pow = V.max_pow
  end : LocalView
    with type data = a
     and type env = b)
;;
