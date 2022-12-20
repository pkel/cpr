open Cpr_lib

module type Parameters = sig
  include Bk.Parameters
  include Nakamoto_ssz.Parameters
end

module Make (Parameters : Parameters) = struct
  open Parameters
  module Protocol = Bk.Make (Parameters)
  open Protocol

  let key = Format.asprintf "ssz-%s" (if unit_observation then "unitobs" else "rawobs")

  let info =
    Format.asprintf
      "SSZ'16-like attack space with %s observations"
      (if unit_observation then "unit" else "raw")
  ;;

  module Observation = struct
    type t =
      { public_blocks : int (** number of public blocks after common ancestor *)
      ; private_blocks : int (** number of private blocks after common ancestor *)
      ; diff_blocks : int (** private_blocks - public_blocks *)
      ; public_votes : int
            (** number of public votes confirming the public leading block *)
      ; private_votes_inclusive : int
            (** number of votes confirming the private leading block *)
      ; private_votes_exclusive : int
            (** number of private votes confirming the private leading block *)
      ; lead : bool (** attacker is truthful leader on leading public block *)
      ; event : [ `Append | `ProofOfWork | `Network ] (* What is currently going on? *)
      }
    [@@deriving fields]

    module Normalizers = struct
      open Ssz_tools.NormalizeObs

      let public_blocks = UnboundedInt { non_negative = true; scale = 1 }
      let private_blocks = UnboundedInt { non_negative = true; scale = 1 }
      let diff_blocks = UnboundedInt { non_negative = false; scale = 1 }
      let public_votes = UnboundedInt { non_negative = true; scale = k }
      let private_votes_inclusive = UnboundedInt { non_negative = true; scale = k }
      let private_votes_exclusive = UnboundedInt { non_negative = true; scale = k }
      let lead = Bool
      let event = Discrete [ `Append; `ProofOfWork; `Network ]
    end

    let length = List.length Fields.names

    let low, high =
      let low, high = Float.Array.make length 0., Float.Array.make length 0. in
      let set spec i _field =
        let l, h = Ssz_tools.NormalizeObs.range ~unit:unit_observation spec in
        Float.Array.set low i l;
        Float.Array.set high i h;
        i + 1
      in
      let _ =
        let open Normalizers in
        Fields.fold
          ~init:0
          ~public_blocks:(set public_blocks)
          ~private_blocks:(set private_blocks)
          ~diff_blocks:(set diff_blocks)
          ~public_votes:(set public_votes)
          ~private_votes_inclusive:(set private_votes_inclusive)
          ~private_votes_exclusive:(set private_votes_exclusive)
          ~lead:(set lead)
          ~event:(set event)
      in
      low, high
    ;;

    let to_floatarray t =
      let a = Float.Array.make length Float.nan in
      let set spec i field =
        Float.Array.set
          a
          i
          (Fieldslib.Field.get field t
          |> Ssz_tools.NormalizeObs.to_float ~unit:unit_observation spec);
        i + 1
      in
      let _ =
        let open Normalizers in
        Fields.fold
          ~init:0
          ~public_blocks:(set public_blocks)
          ~private_blocks:(set private_blocks)
          ~diff_blocks:(set diff_blocks)
          ~public_votes:(set public_votes)
          ~private_votes_inclusive:(set private_votes_inclusive)
          ~private_votes_exclusive:(set private_votes_exclusive)
          ~lead:(set lead)
          ~event:(set event)
      in
      a
    ;;

    let of_floatarray =
      let get spec _ i =
        ( (fun a ->
            Float.Array.get a i
            |> Ssz_tools.NormalizeObs.of_float ~unit:unit_observation spec)
        , i + 1 )
      in
      let open Normalizers in
      fst
        (Fields.make_creator
           0
           ~public_blocks:(get public_blocks)
           ~private_blocks:(get private_blocks)
           ~diff_blocks:(get diff_blocks)
           ~public_votes:(get public_votes)
           ~private_votes_inclusive:(get private_votes_inclusive)
           ~private_votes_exclusive:(get private_votes_exclusive)
           ~lead:(get lead)
           ~event:(get event))
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
      let event = conv Ssz_tools.event_to_string in
      Fields.to_list
        ~public_blocks:int
        ~private_blocks:int
        ~diff_blocks:int
        ~public_votes:int
        ~private_votes_inclusive:int
        ~private_votes_exclusive:int
        ~lead:bool
        ~event
      |> String.concat "\n"
    ;;
  end

  module Action = Ssz_tools.Action8

  module Agent (V : View with type data = data) = struct
    open Protocol.Referee (V)
    include V
    module N = Honest (V)

    type state =
      | BetweenActions of
          { public : block (* defender's preferred block *)
          ; private_ : block (* attacker's preferred block *)
          ; pending_private_to_public_messages :
              block list (* messages sent with last action *)
          }

    type state_before_action =
      | BeforeAction of
          { public : block
          ; private_ : block
          }

    type observable_state =
      | Observable of
          { public : block
          ; private_ : block
          ; common : block
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

    module Dagtools = Dagtools.Make (Block)

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
      let common = Dagtools.common_ancestor public private_ |> Option.get in
      Observable { public; private_; common; event }
    ;;

    let prepare state = deliver_private_to_public_messages state |> prepare

    let observe (Observable state) =
      let open Observation in
      let public_votes =
        children state.public
        |> List.filter public_visibility
        |> List.filter is_vote
        |> List.length
      and private_votes_inclusive =
        children state.private_ |> List.filter is_vote |> List.length
      and private_votes_exclusive =
        children state.private_
        |> List.filter is_vote
        |> List.filter N.appended_by_me
        |> List.length
      in
      let lead =
        match children state.public |> List.filter is_vote with
        | [] -> false
        | votes ->
          let leader =
            Compare.first Compare.(by compare_pow (fun n -> pow n |> Option.get)) 1 votes
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
      ; event = state.event
      }
    ;;

    let apply (Observable state) action =
      let parent_block x =
        match parents x with
        | hd :: _ when is_block hd -> Some hd
        | _ -> None
      in
      let release kind =
        let height, nvotes =
          let height = block_height_exn state.public
          and nvotes =
            children state.public
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
          h state.private_
          (* NOTE: if private height is smaller public height, then private head is marked
             for release. *)
        in
        (* include proposal if attacker was able to produce one *)
        let block, nvotes =
          if nvotes >= k
          then (
            match children block |> List.filter is_block with
            | proposal :: _ -> proposal, 0
            | [] -> block, nvotes)
          else block, nvotes
        in
        let votes = children block |> List.filter is_vote in
        match Compare.first Compare.(by float visible_since) nvotes votes with
        | Some subset -> block :: subset
        | None ->
          (* not enough votes, release all *)
          block :: votes
      in
      let share, private_ =
        match (action : Action.t) with
        | Adopt_Proceed | Adopt_Prolong -> [], state.public
        | Match_Proceed | Match_Prolong -> release `Match, state.private_
        | Override_Proceed | Override_Prolong -> release `Override, state.private_
        | Wait_Proceed | Wait_Prolong -> [], state.private_
      in
      let append =
        let vote_filter =
          match action with
          | Adopt_Proceed | Override_Proceed | Match_Proceed | Wait_Proceed ->
            vote_filter `Inclusive
          | Adopt_Prolong | Override_Prolong | Match_Prolong | Wait_Prolong ->
            vote_filter `Exclusive
        in
        match N.propose ~vote_filter private_ with
        | Some block -> [ block ]
        | None -> []
      in
      BetweenActions
        { public = state.public; private_; pending_private_to_public_messages = share }
      |> return ~append ~share
    ;;
  end

  let attacker (type a) policy ((module V) : (a, data) view) : (a, data) node =
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
