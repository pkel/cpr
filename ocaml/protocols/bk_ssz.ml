open Cpr_lib.Next

module Make (Parameters : Bk.Parameters) = struct
  open Parameters
  module Protocol = Bk.Make (Parameters)
  open Protocol

  let key = "ssz"
  let info = "SSZ'16-like attack space"

  module Observation = struct
    type t =
      { public_blocks : int (** number of blocks after common ancestor *)
      ; public_votes : int (** number of votes confirming the leading block *)
      ; private_blocks : int (** number of blocks after common ancestor *)
      ; private_votes : int (** number of votes confirming the leading block *)
      ; diff_blocks : int (** private_blocks - public_blocks *)
      ; diff_votes : int (** private_votes - public_votes *)
      ; lead : bool (** attacker is truthful leader on leading public block *)
      }
    [@@deriving fields]

    let length = List.length Fields.names

    let low =
      { public_blocks = 0
      ; public_votes = 0
      ; private_blocks = 0
      ; private_votes = 0
      ; diff_blocks = min_int
      ; diff_votes = min_int
      ; lead = false
      }
    ;;

    let high =
      { public_blocks = max_int
      ; public_votes = max_int
      ; private_blocks = max_int
      ; private_votes = max_int
      ; diff_blocks = max_int
      ; diff_votes = max_int
      ; lead = true
      }
    ;;

    let to_floatarray t =
      let a = Float.Array.make length Float.nan in
      let set conv i field =
        Float.Array.set a i (Fieldslib.Field.get field t |> conv);
        i + 1
      in
      let int = set float_of_int
      and bool = set (fun x -> if x then 1. else 0.) in
      let _ =
        Fields.fold
          ~init:0
          ~public_blocks:int
          ~public_votes:int
          ~private_blocks:int
          ~private_votes:int
          ~diff_blocks:int
          ~diff_votes:int
          ~lead:bool
      in
      a
    ;;

    let of_floatarray =
      let get conv _ i = (fun a -> Float.Array.get a i |> conv), i + 1 in
      let int = get int_of_float
      and bool =
        get (fun f ->
            match int_of_float f with
            | 0 -> false
            | _ -> true)
      in
      fst
        (Fields.make_creator
           0
           ~public_blocks:int
           ~public_votes:int
           ~private_blocks:int
           ~private_votes:int
           ~diff_blocks:int
           ~diff_votes:int
           ~lead:bool)
    ;;

    let to_string t =
      let conv to_s field =
        Printf.sprintf
          "%s: %s"
          (Fieldslib.Field.name field)
          (to_s (Fieldslib.Field.get field t))
      in
      let int = conv string_of_int
      and bool = conv string_of_bool in
      Fields.to_list
        ~public_blocks:int
        ~public_votes:int
        ~private_blocks:int
        ~private_votes:int
        ~diff_blocks:int
        ~diff_votes:int
        ~lead:bool
      |> String.concat "\n"
    ;;

    let%test _ =
      let run _i =
        let t =
          { public_blocks = Random.bits ()
          ; public_votes = Random.bits ()
          ; private_blocks = Random.bits ()
          ; private_votes = Random.bits ()
          ; diff_blocks = Random.bits ()
          ; diff_votes = Random.bits ()
          ; lead = Random.bool ()
          }
        in
        t = (to_floatarray t |> of_floatarray)
      in
      List.init 50 run |> List.for_all (fun x -> x)
    ;;
  end

  module Action = struct
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

  module Agent (V : LocalView with type data = data) = struct
    open Protocol.Referee (V)
    include V

    module State : sig
      type t = private
        { public : env Dag.vertex (* defender's preferred block *)
        ; private_ : env Dag.vertex (* attacker's preferred block *)
        ; common : env Dag.vertex (* common chain *)
        ; epoch : [ `Proceed | `Prolong ]
              (* Proceed: the attacker considers the defender's votes that extend on his
                 preferred block when building a new block.

                 Prolong: the attacker prolongs the current epoch until he can form a
                 block that does not reference any defender votes. *)
        }

      val init : epoch:[ `Proceed | `Prolong ] -> env Dag.vertex -> t

      (* Set fields in state; updates common chain *)
      val update
        :  ?public:env Dag.vertex
        -> ?private_:env Dag.vertex
        -> ?epoch:[ `Proceed | `Prolong ]
        -> t
        -> t
    end = struct
      type t =
        { public : env Dag.vertex
        ; private_ : env Dag.vertex
        ; common : env Dag.vertex
        ; epoch : [ `Proceed | `Prolong ]
        }

      let init ~epoch x = { public = x; private_ = x; common = x; epoch }

      (* call this whenever public or private_ changes *)
      let set_common state =
        let common = Dag.common_ancestor view state.public state.private_ in
        assert (Option.is_some common) (* all our protocols maintain this invariant *);
        { state with common = Option.get common }
      ;;

      let update ?public ?private_ ?epoch t =
        set_common
          { public = Option.value ~default:t.public public
          ; private_ = Option.value ~default:t.private_ private_
          ; epoch = Option.value ~default:t.epoch epoch
          ; common = t.common
          }
      ;;
    end

    type state = State.t
    type observable_state = Observable of state

    let preferred (s : state) = s.private_

    let init ~roots =
      let module N = Honest (V) in
      N.init ~roots |> State.init ~epoch:`Prolong
    ;;

    (* the attacker emulates a defending node. This is the local_view of the defender *)

    let public_visibility _state x = released x

    let public_view s : (env, data) local_view =
      (module struct
        include V

        let view = Dag.filter (public_visibility s) view
        let appended_by_me _vertex = false

        (* The attacker simulates an honest node on the public view. This node should not
           interpret attacker vertices as own vertices. *)
      end)
    ;;

    (* the attacker works on a subset of the total information: he ignores new defender
       blocks *)

    let private_visibility (s : state) vertex =
      (* defender votes for the attacker's preferred block *)
      (* || anything mined by the attacker *)
      (* || anything on the common chain *)
      (s.epoch = `Proceed && is_vote vertex && last_block vertex $== s.private_)
      || appended_by_me vertex
      || Dag.partial_order s.common vertex >= 0
    ;;

    let private_view (s : state) : _ local_view =
      (module struct
        include V

        let view = Dag.filter (private_visibility s) view
      end)
    ;;

    (* the attacker emulates a defending node. This describes the defender node *)
    let handle_public (s : state) event =
      let (module V) = public_view s in
      let open Honest (V) in
      let public = (handler s.public event).state in
      State.update ~public s
    ;;

    (* this describes the attacker node *)
    let handle_private (s : state) event =
      let (module V) = private_view s in
      let open Honest (V) in
      let private_ = (handler s.private_ event).state in
      State.update ~private_ s
    ;;

    let puzzle_payload (s : state) =
      let (module Private) = private_view s in
      let open Honest (Private) in
      puzzle_payload s.private_
    ;;

    let prepare state event =
      match event with
      | PuzzleSolved _ ->
        (* work on private chain *)
        handle_private state event
      | Deliver x ->
        let state =
          (* simulate defender *)
          handle_public state event
        in
        (* deliver visible (not ignored) messages *)
        if private_visibility state x then handle_private state event else state
    ;;

    let prepare s e = Observable (prepare s e)

    let observe (Observable s) =
      let open Observation in
      let (module Private) = private_view s in
      let (module Public) = public_view s in
      let private_votes =
        Dag.children Private.view s.private_ |> List.filter is_vote |> List.length
      and public_votes =
        Dag.children Public.view s.public |> List.filter is_vote |> List.length
      in
      let lead =
        match Dag.children view s.public |> List.filter is_vote with
        | [] -> false
        | votes ->
          let leader =
            first Compare.(by (tuple int int) (fun n -> pow_hash n |> Option.get)) 1 votes
            |> Option.get
            |> List.hd
          in
          appended_by_me leader
      and ca = last_block s.common in
      let ca_height = block_height_exn ca
      and private_height = block_height_exn s.private_
      and public_height = block_height_exn s.public in
      { private_blocks = private_height - ca_height
      ; public_blocks = public_height - ca_height
      ; diff_blocks = private_height - public_height
      ; private_votes
      ; public_votes
      ; diff_votes = private_votes - public_votes
      ; lead
      }
    ;;

    let interpret (s : state) action =
      let parent_block x =
        match Dag.parents view x with
        | hd :: _ when is_block hd -> Some hd
        | _ -> None
      in
      let (module Public) = public_view s in
      let (module Private) = private_view s in
      let release kind =
        let height, nvotes =
          let height = block_height_exn s.public
          and nvotes =
            List.length (Dag.children Public.view s.public |> List.filter is_vote)
          in
          match kind with
          | `Match -> height, nvotes
          | `Override -> if nvotes >= k then height + 1, 0 else height, nvotes + 1
        in
        let block =
          (* find block to be released backwards from private head *)
          let rec h b =
            if block_height_exn b <= height then b else parent_block b |> Option.get |> h
          in
          h s.private_
          (* NOTE: if private height is smaller public height, then private head is marked
             for release. *)
        in
        (* include proposal if attacker was able to produce one *)
        let block, nvotes =
          if nvotes >= k
          then (
            match Dag.children Private.view block |> List.filter is_block with
            | proposal :: _ -> proposal, 0
            | [] -> block, nvotes)
          else block, nvotes
        in
        let votes = Dag.children Private.view block |> List.filter is_vote in
        match first Compare.(by float delivered_at) nvotes votes with
        | Some subset -> block :: subset
        | None ->
          (* not enough votes, release all *)
          block :: votes
      in
      match (action : Action.t) with
      | Adopt_Proceed -> [], State.update ~epoch:`Proceed ~private_:s.public s
      | Adopt_Prolong -> [], State.update ~epoch:`Prolong ~private_:s.public s
      | Match_Proceed -> release `Match, State.update ~epoch:`Proceed s
      | Match_Prolong -> release `Match, State.update ~epoch:`Prolong s
      | Override_Proceed -> release `Override, State.update ~epoch:`Proceed s
      | Override_Prolong -> release `Override, State.update ~epoch:`Prolong s
      | Wait_Proceed -> [], State.update ~epoch:`Proceed s
      | Wait_Prolong -> [], State.update ~epoch:`Prolong s
    ;;

    let conclude (share, state) =
      let simulate_public state msg = handle_public state (Deliver msg) in
      { share; state = List.fold_left simulate_public state share }
    ;;

    let apply (Observable state) action = interpret state action |> conclude
  end

  let attacker (type a) policy ((module V) : (a, data) local_view) : (a, data) node =
    Node
      (module struct
        include Agent (V)

        let handler s e =
          let s = prepare s e in
          observe s |> policy |> apply s
        ;;
      end)
  ;;

  module Policies = struct
    let honest o =
      let open Observation in
      let open Action in
      if o.public_blocks > 0 then Adopt_Proceed else Override_Proceed
    ;;

    let selfish o =
      (* Ad-hoc strategy. This is probably not optimal. *)
      let open Observation in
      let open Action in
      if o.private_blocks < o.public_blocks
      then Adopt_Proceed
      else if o.private_blocks = 0 && o.public_blocks = 0
      then Wait_Prolong
      else if o.public_blocks = 0
      then Wait_Proceed
      else Override_Proceed
    ;;
  end

  let policies =
    let open Collection in
    let open Policies in
    empty
    |> add ~info:"emulate honest behaviour" "honest" honest
    |> add ~info:"ad-hoc selfish policy" "selfish" selfish
  ;;
end