open Intf

module Make (M : sig
  module AttackSpace : AttackSpace

  val alpha : float
  val gamma : float
end) =
struct
  open M

  type actor =
    [ `A
    | `D
    ]

  type data =
    { prot : AttackSpace.Protocol.data
    ; appended_by : actor list
    ; pow : bool
    ; signed : bool
    ; public : bool
    }

  let dag_ref = ref (Pdag.empty ())
  let filter_ref = ref (Fun.const true)
  let actor_ref : actor ref = ref `A

  module AtkView = struct
    type block = Pdag.vertex_id
    type data = AttackSpace.Protocol.data

    let children x = Pdag.children !dag_ref x |> List.filter !filter_ref
    let parents x = Pdag.parents !dag_ref x |> List.filter !filter_ref

    type pow = int

    let pow x =
      let d = Pdag.data !dag_ref x in
      if d.pow then Some 0 else None
    ;;

    let compare_pow = compare
    let min_pow = 0
    let max_pow = 0

    let signature x =
      let d = Pdag.data !dag_ref x in
      if d.signed
      then (
        match List.hd d.appended_by with
        | `A -> Some 0
        | `D -> Some 1)
      else None
    ;;

    let data x = (Pdag.data !dag_ref x).prot

    module Block = struct
      type t = block

      let children = children
      let parents = parents
      let eq = Pdag.vertex_eq
      let neq = Pdag.vertex_neq
      let compare = Pdag.compare_vertex
      let partial_compare = Pdag.partial_order
    end

    let raise_invalid_dag _ = assert false
    let visible_since _x = assert false

    let visibility x =
      let d = Pdag.data !dag_ref x in
      if List.mem !actor_ref d.appended_by
      then if d.public then `Released else `Withheld
      else if d.public
      then `Received
      else assert false
    ;;

    let my_id = 0
  end

  module _ : LocalView = AtkView

  module DefView = struct
    include AtkView

    let my_id = 1
  end

  module _ : LocalView = DefView

  module Def = AttackSpace.Protocol.Honest (struct
    type block = Pdag.vertex_id
  end)

  module Agent = AttackSpace.Agent (AtkView)
end
