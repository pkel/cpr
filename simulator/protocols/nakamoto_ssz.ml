open Cpr_lib
module Protocol = Nakamoto
open Protocol

let key = "ssz"
let info = "SSZ'16 attack space"

module Observation = struct
  type t =
    { public_blocks : int (** number of public blocks after common ancestor *)
    ; private_blocks : int (** number of private blocks after common ancestor *)
    ; diff_blocks : int (** private_blocks - public_blocks *)
    ; event : [ `ProofOfWork | `Network ] (* What is currently going on? *)
    }
  [@@deriving fields]

  module Normalizers = struct
    open Ssz_tools.NormalizeObs

    let public_blocks = UnboundedInt { non_negative = true; scale = 1 }
    let private_blocks = UnboundedInt { non_negative = true; scale = 1 }
    let diff_blocks = UnboundedInt { non_negative = false; scale = 1 }
    let event = Discrete [ `ProofOfWork; `Network ]
  end

  let length = List.length Fields.names

  let to_floatarray t =
    let a = Float.Array.make length Float.nan in
    let set spec i field =
      Float.Array.set
        a
        i
        (Fieldslib.Field.get field t |> Ssz_tools.NormalizeObs.to_float spec);
      i + 1
    in
    let _ =
      let open Normalizers in
      Fields.fold
        ~init:0
        ~public_blocks:(set public_blocks)
        ~private_blocks:(set private_blocks)
        ~diff_blocks:(set diff_blocks)
        ~event:(set event)
    in
    a
  ;;

  let of_floatarray =
    let get spec _ i =
      (fun a -> Float.Array.get a i |> Ssz_tools.NormalizeObs.of_float spec), i + 1
    in
    let open Normalizers in
    fst
      (Fields.make_creator
         0
         ~public_blocks:(get public_blocks)
         ~private_blocks:(get private_blocks)
         ~diff_blocks:(get diff_blocks)
         ~event:(get event))
  ;;

  let to_string t =
    let conv to_s field =
      Printf.sprintf
        "%s: %s"
        (Fieldslib.Field.name field)
        (to_s (Fieldslib.Field.get field t))
    in
    let int = conv string_of_int in
    let event = conv Ssz_tools.event_to_string in
    Fields.to_list ~public_blocks:int ~private_blocks:int ~diff_blocks:int ~event
    |> String.concat "\n"
  ;;
end

module Action = struct
  type t =
    | Adopt
        (** Adopt the defender's preferred chain as attacker's preferred chain. Withheld
            attacker blocks are discarded.

            Equivalent to SSZ'16 model. *)
    | Override
        (** Publish just enough information to make the defender adopt the chain just
            released. The attacker continues mining the private chain.

            If override is impossible, this still results in a release of withheld
            information.

            Equivalent to SSZ'16 model. *)
    | Match
        (** Publish just enough information such that the defender observes a tie between
            two chains. The attacker continues mining the private chain.

            If match is impossible, this still results in a release of withheld
            information.

            Equivalent to SSZ'16 model. *)
    | Wait (** Continue withholding. Always possible. Equivalent to SSZ'16 model. *)
  [@@deriving variants]

  let to_string = Variants.to_name
  let to_int = Variants.to_rank

  let table =
    let add acc var = var.Variantslib.Variant.constructor :: acc in
    Variants.fold ~init:[] ~adopt:add ~override:add ~match_:add ~wait:add
    |> List.rev
    |> Array.of_list
  ;;

  let of_int i = table.(i)
  let n = Array.length table
end

module Agent (V : View with type data = data) = struct
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
        ; event : [ `ProofOfWork | `Network ]
        }

  let init ~roots =
    let root = N.init ~roots in
    BetweenActions
      { private_ = root; public = root; pending_private_to_public_messages = [] }
  ;;

  let preferred (BetweenActions s) = s.private_
  let puzzle_payload (BetweenActions s) = N.puzzle_payload s.private_

  let deliver_private_to_public_messages (BetweenActions state) =
    let public =
      List.fold_left
        (fun old consider -> N.update_head ~old consider)
        state.public
        state.pending_private_to_public_messages
    in
    BeforeAction { public; private_ = state.private_ }
  ;;

  module Dagtools = Dagtools.Make (struct
    include V

    type vertex = block

    let eq = block_eq
    let neq = block_neq
  end)

  let prepare (BeforeAction state) event =
    let public, private_, event =
      match event with
      | Append _ -> failwith "not implemented"
      | Network x ->
        (* simulate defender *)
        N.update_head ~old:state.public x, state.private_, `Network
      | ProofOfWork x ->
        (* work on private chain *)
        state.public, x, `ProofOfWork
    in
    let common = Dagtools.common_ancestor public private_ |> Option.get in
    Observable { public; private_; common; event }
  ;;

  let prepare state = deliver_private_to_public_messages state |> prepare

  let observe (Observable state) =
    let open Observation in
    let ca_height = data state.common |> height
    and private_height = data state.private_ |> height
    and public_height = data state.public |> height in
    { private_blocks = private_height - ca_height
    ; public_blocks = public_height - ca_height
    ; diff_blocks = private_height - public_height
    ; event = state.event
    }
  ;;

  let apply (Observable state) action =
    let parent vtx =
      match parents vtx with
      | [ x ] -> Some x
      | _ -> None
    in
    let match_ offset =
      let h =
        (* height of to be released block *)
        height (data state.public) + offset
      in
      (* look for to be released block backwards from private head *)
      let rec f b = if height (data b) <= h then b else parent b |> Option.get |> f in
      [ f state.private_ ]
      (* NOTE: if private height is smaller target height, then private head is
         released. *)
    in
    let share, private_ =
      match (action : Action.t) with
      | Adopt -> [], state.public
      | Match -> match_ 0, state.private_
      | Override -> match_ 1, state.private_
      | Wait -> [], state.private_
    in
    BetweenActions
      { private_; public = state.public; pending_private_to_public_messages = share }
    |> return ~share
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
    if o.private_blocks > o.public_blocks
    then Override
    else if o.private_blocks < o.public_blocks
    then Adopt
    else Wait
  ;;

  (* Patrik's ad-hoc strategy *)
  let simple o =
    let open Observation in
    let open Action in
    if o.public_blocks > 0
    then if o.private_blocks < o.public_blocks then Adopt else Override
    else Wait
  ;;

  (* Eyal and Sirer. Majority is not enough: Bitcoin mining is vulnerable. 2014. *)
  let es_2014 o =
    (* I interpret this from the textual description of the strategy. There is an
       algorithmic version in the paper, but it depends on the observation whether the
       last mined block is honest or not. *)
    let open Observation in
    let open Action in
    if o.private_blocks < o.public_blocks
    then (* 1. *) Adopt
    else if o.public_blocks = 0 && o.private_blocks = 1
    then (* 2. *) Wait
    else if o.public_blocks = 1 && o.private_blocks = 1
    then (* 3. *) Match
    else if o.public_blocks = 1 && o.private_blocks = 2
    then (* 4. *) Override
    else if o.public_blocks = 2 && o.private_blocks = 1
    then (* 5. Redundant: included in 1. *)
      Adopt
    else (
      (* The attacker established a lead of more than two before: *)
      let _ = () in
      if o.public_blocks > 0
      then
        if o.private_blocks - o.public_blocks = 1
        then (* 6. *) Override
        else (* 7. *) Match
      else Wait)
  ;;

  (* Sapirshtein, Sompolinsky, Zohar. Optimal Selfish Mining Strategies in Bitcoin.
     2016. *)
  let ssz_2016_sm1 o =
    (* The authors rephrase the policy of ES'14 and call it SM1. Their version is much
       shorter.

       The authors define an MDP to find better strategies for various parameters alpha
       and gamma. We cannot reproduce this here in this module. Our RL framework should be
       able to find these policies, though. *)
    let open Observation in
    let open Action in
    match o.public_blocks, o.private_blocks with
    | h, a when h > a -> Adopt
    | 1, 1 -> Match
    | h, a when h = a - 1 && h >= 1 -> Override
    | _ (* Otherwise *) -> Wait
  ;;
end

let policies =
  let open Collection in
  let open Policies in
  empty
  |> add ~info:"emulate honest behaviour" "honest" honest
  |> add ~info:"simple withholding policy" "simple" simple
  |> add ~info:"Eyal and Sirer 2014" "eyal-sirer-2014" es_2014
  |> add ~info:"Sapirshtein et al. 2016, SM1" "sapirshtein-2016-sm1" ssz_2016_sm1
;;
