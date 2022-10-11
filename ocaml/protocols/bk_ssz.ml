open Cpr_lib

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
      ; include_foreign_votes : bool
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
      ; include_foreign_votes = false
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
      ; include_foreign_votes = true
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
          ~include_foreign_votes:bool
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
           ~lead:bool
           ~include_foreign_votes:bool)
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
        ~include_foreign_votes:bool
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
          ; include_foreign_votes = Random.bool ()
          }
        in
        t = (to_floatarray t |> of_floatarray)
      in
      List.init 50 run |> List.for_all (fun x -> x)
    ;;
  end

  module Action = Ssz_tools.Action8

  module Agent (V : LocalView with type data = data) = struct
    open Protocol.Referee (V)
    include V
    module State = Ssz_tools.State8 (V)

    type state = State.t
    type observable_state = Observable of (state * (env, data) draft_vertex list)

    let preferred (s : state) = s.private_

    let init ~roots =
      let module N = Honest (V) in
      N.init ~roots |> State.init ~epoch:`Prolong
    ;;

    (* the attacker emulates a defending node. This is the local_view of the defender *)

    let public_visibility (_s : state) x =
      match visibility x with
      | `Withheld -> false
      | `Received | `Released -> true
    ;;

    let public_view s : (env, data) local_view =
      Ssz_tools.emulated_view
        ~pretend_not_me:true
        ~filter:(public_visibility s)
        (module V)
    ;;

    (* the attacker works on a subset of the total information: he ignores new defender
       blocks *)

    let private_visibility (s : state) x =
      (* defender votes for the attacker's preferred block *)
      (* || anything mined by the attacker *)
      (* || anything on the common chain *)
      (s.epoch = `Proceed && is_vote x && last_block x $== s.private_)
      || Dag.partial_order s.common x >= 0
      ||
      match visibility x with
      | `Withheld | `Released -> true
      | `Received -> false
    ;;

    let private_view (s : state) : _ local_view =
      Ssz_tools.emulated_view
        ~pretend_not_me:false
        ~filter:(private_visibility s)
        (module V)
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
      let action = handler s.private_ event in
      let private_ = action.state in
      State.update ~private_ s, action.append
    ;;

    let puzzle_payload (s : state) =
      let (module Private) = private_view s in
      let open Honest (Private) in
      puzzle_payload s.private_
    ;;

    let prepare (state : state) event =
      let state =
        let pending = state.pending_private_to_public_messages in
        List.fold_left
          (fun state msg ->
            assert (public_visibility state msg);
            handle_public state (Network msg))
          (State.update ~pending_private_to_public_messages:[] state)
          pending
      in
      match event with
      | Append x | ProofOfWork x | Network x ->
        let state =
          if public_visibility state x then handle_public state event else state
        in
        (* deliver visible (not ignored) messages *)
        if private_visibility state x then handle_private state event else state, []
    ;;

    let prepare s e = Observable (prepare s e)

    let observe (Observable (s, _append)) =
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
            Compare.first
              Compare.(by (tuple int int) (fun n -> pow n |> Option.get))
              1
              votes
            |> Option.get
            |> List.hd
          in
          signature leader = Some my_id
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
      ; include_foreign_votes =
          (match s.epoch with
          | `Proceed -> true
          | `Prolong -> false)
      }
    ;;

    let interpret (Observable (s, append)) action =
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
        match Compare.first Compare.(by float visible_since) nvotes votes with
        | Some subset -> block :: subset
        | None ->
          (* not enough votes, release all *)
          block :: votes
      in
      let b, c =
        match (action : Action.t) with
        | Adopt_Proceed -> [], State.update ~epoch:`Proceed ~private_:s.public s
        | Adopt_Prolong -> [], State.update ~epoch:`Prolong ~private_:s.public s
        | Match_Proceed -> release `Match, State.update ~epoch:`Proceed s
        | Match_Prolong -> release `Match, State.update ~epoch:`Prolong s
        | Override_Proceed -> release `Override, State.update ~epoch:`Proceed s
        | Override_Prolong -> release `Override, State.update ~epoch:`Prolong s
        | Wait_Proceed -> [], State.update ~epoch:`Proceed s
        | Wait_Prolong -> [], State.update ~epoch:`Prolong s
      in
      append, b, c
    ;;

    let conclude (append, pending_private_to_public_messages, state) =
      State.update ~pending_private_to_public_messages state
      |> return ~share:pending_private_to_public_messages ~append
    ;;

    let apply state action = interpret state action |> conclude
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
