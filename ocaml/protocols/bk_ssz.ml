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
      ; public_votes : int
            (** number of public votes confirming the public leading block *)
      ; private_blocks : int (** number of blocks after common ancestor *)
      ; private_votes_inclusive : int
            (** number of votes confirming the private leading block *)
      ; private_votes_exclusive : int
            (** number of private votes confirming the private leading block *)
      ; diff_blocks : int (** private_blocks - public_blocks *)
      ; lead : bool (** attacker is truthful leader on leading public block *)
      ; event : int
      }
    [@@deriving fields]

    let length = List.length Fields.names

    let low =
      { public_blocks = 0
      ; public_votes = 0
      ; private_blocks = 0
      ; private_votes_inclusive = 0
      ; private_votes_exclusive = 0
      ; diff_blocks = min_int
      ; lead = false
      ; event = Ssz_tools.Event.low
      }
    ;;

    let high =
      { public_blocks = max_int
      ; public_votes = max_int
      ; private_blocks = max_int
      ; private_votes_inclusive = max_int
      ; private_votes_exclusive = max_int
      ; diff_blocks = max_int
      ; lead = true
      ; event = Ssz_tools.Event.high
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
          ~private_votes_inclusive:int
          ~private_votes_exclusive:int
          ~diff_blocks:int
          ~lead:bool
          ~event:int
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
           ~private_votes_inclusive:int
           ~private_votes_exclusive:int
           ~diff_blocks:int
           ~lead:bool
           ~event:int)
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
        ~private_votes_inclusive:int
        ~private_votes_exclusive:int
        ~diff_blocks:int
        ~lead:bool
        ~event:int
      |> String.concat "\n"
    ;;

    let%test _ =
      let run _i =
        let t =
          { public_blocks = Random.bits ()
          ; public_votes = Random.bits ()
          ; private_blocks = Random.bits ()
          ; private_votes_inclusive = Random.bits ()
          ; private_votes_exclusive = Random.bits ()
          ; diff_blocks = Random.bits ()
          ; lead = Random.bool ()
          ; event = Random.bits ()
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
    module N = Honest (V)

    type state =
      | BetweenActions of
          { public : env Dag.vertex (* defender's preferred block *)
          ; private_ : env Dag.vertex (* attacker's preferred block *)
          ; pending_private_to_public_messages :
              env Dag.vertex list (* messages sent with last action *)
          }

    type state_before_action =
      | BeforeAction of
          { public : env Dag.vertex
          ; private_ : env Dag.vertex
          }

    type observable_state =
      | Observable of
          { public : env Dag.vertex
          ; private_ : env Dag.vertex
          ; common : env Dag.vertex
          ; event : [ `Append | `ProofOfWork | `Network ]
          }

    let init ~roots =
      let root = N.init ~roots in
      BetweenActions
        { private_ = root; public = root; pending_private_to_public_messages = [] }
    ;;

    let preferred (BetweenActions state) = state.private_

    let vote_filter = function
      | `Inclusive -> Fun.const true
      | `Exclusive -> N.appended_by_me
    ;;

    let puzzle_payload (BetweenActions state) = N.puzzle_payload state.private_

    let public_visibility x =
      match visibility x with
      | `Released | `Received -> true
      | `Withheld -> false
    ;;

    let deliver_private_to_public_messages (BetweenActions state) =
      let public =
        let vote_filter = public_visibility in
        List.fold_left
          (fun old consider -> N.update_head ~vote_filter ~old (last_block consider))
          state.public
          state.pending_private_to_public_messages
      in
      BeforeAction { public; private_ = state.private_ }
    ;;

    let prepare (BeforeAction state) event =
      let public, private_, event =
        match event with
        | Append x -> state.public, x, `Append
        | ProofOfWork _x -> state.public, state.private_, `ProofOfWork
        | Network x ->
          let b = last_block x
          and vote_filter = public_visibility in
          N.update_head ~vote_filter ~old:state.public b, state.private_, `Network
      in
      let common = Dag.common_ancestor view public private_ |> Option.get in
      Observable { public; private_; common; event }
    ;;

    let prepare state = deliver_private_to_public_messages state |> prepare

    let observe (Observable state) =
      let open Observation in
      let public_votes =
        Dag.children view state.public
        |> List.filter public_visibility
        |> List.filter is_vote
        |> List.length
      and private_votes_inclusive =
        Dag.children view state.private_ |> List.filter is_vote |> List.length
      and private_votes_exclusive =
        Dag.children view state.private_
        |> List.filter is_vote
        |> List.filter N.appended_by_me
        |> List.length
      in
      let lead =
        match Dag.children view state.public |> List.filter is_vote with
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
      and ca = last_block state.common in
      let ca_height = block_height_exn ca
      and private_height = block_height_exn state.private_
      and public_height = block_height_exn state.public in
      { private_blocks = private_height - ca_height
      ; public_blocks = public_height - ca_height
      ; diff_blocks = private_height - public_height
      ; private_votes_inclusive
      ; private_votes_exclusive
      ; public_votes
      ; lead
      ; event = Ssz_tools.Event.to_int state.event
      }
    ;;

    let apply (Observable s) action =
      let parent_block x =
        match Dag.parents view x with
        | hd :: _ when is_block hd -> Some hd
        | _ -> None
      in
      let release kind =
        let height, nvotes =
          let height = block_height_exn s.public
          and nvotes =
            Dag.children view s.public
            |> List.filter public_visibility
            |> List.filter is_vote
            |> List.length
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
            match Dag.children view block |> List.filter is_block with
            | proposal :: _ -> proposal, 0
            | [] -> block, nvotes)
          else block, nvotes
        in
        let votes = Dag.children view block |> List.filter is_vote in
        match Compare.first Compare.(by float visible_since) nvotes votes with
        | Some subset -> block :: subset
        | None ->
          (* not enough votes, release all *)
          block :: votes
      in
      let share, private_ =
        match (action : Action.t) with
        | Adopt_Proceed | Adopt_Prolong -> [], s.public
        | Match_Proceed | Match_Prolong -> release `Match, s.private_
        | Override_Proceed | Override_Prolong -> release `Override, s.private_
        | Wait_Proceed | Wait_Prolong -> [], s.private_
      in
      let append =
        let vote_filter =
          match action with
          | Adopt_Proceed | Override_Proceed | Match_Proceed | Wait_Proceed ->
            vote_filter `Inclusive
          | Adopt_Prolong | Match_Prolong | Override_Prolong | Wait_Prolong ->
            vote_filter `Exclusive
        in
        match N.propose ~vote_filter private_ with
        | Some block -> [ block ]
        | None -> []
      in
      BetweenActions
        { public = s.public; private_; pending_private_to_public_messages = share }
      |> return ~append ~share
    ;;
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
