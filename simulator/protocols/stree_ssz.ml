open Cpr_lib

module Make (Parameters : Tailstorm_ssz.Parameters) = struct
  open Parameters
  module Protocol = Stree.Make (Parameters)
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
      ; public_depth : int (** public vote tree depth *)
      ; private_depth_inclusive : int
            (** private vote tree depth, including public votes *)
      ; private_depth_exclusive : int
            (** private vote tree depth, excluding private votes *)
      ; event : [ `ProofOfWork | `Network ] (* What is currently going on? *)
      }
    [@@deriving fields]

    module Normalizers = struct
      open Parameters
      open Ssz_tools.NormalizeObs

      let public_blocks = UnboundedInt { non_negative = true; scale = 1 }
      let private_blocks = UnboundedInt { non_negative = true; scale = 1 }
      let diff_blocks = UnboundedInt { non_negative = false; scale = 1 }
      let public_votes = UnboundedInt { non_negative = true; scale = k }
      let private_votes_inclusive = UnboundedInt { non_negative = true; scale = k - 1 }
      let private_votes_exclusive = UnboundedInt { non_negative = true; scale = k - 1 }
      let public_depth = UnboundedInt { non_negative = true; scale = k }
      let private_depth_inclusive = UnboundedInt { non_negative = true; scale = k - 1 }
      let private_depth_exclusive = UnboundedInt { non_negative = true; scale = k - 1 }
      let event = Discrete [ `ProofOfWork; `Network ]
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
          ~public_depth:(set public_depth)
          ~private_depth_inclusive:(set private_depth_inclusive)
          ~private_depth_exclusive:(set private_depth_exclusive)
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
          ~public_depth:(set public_depth)
          ~private_depth_inclusive:(set private_depth_inclusive)
          ~private_depth_exclusive:(set private_depth_exclusive)
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
           ~public_depth:(get public_depth)
           ~private_depth_inclusive:(get private_depth_inclusive)
           ~private_depth_exclusive:(get private_depth_exclusive)
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
      Fields.to_list
        ~public_blocks:int
        ~private_blocks:int
        ~diff_blocks:int
        ~public_votes:int
        ~private_votes_inclusive:int
        ~private_votes_exclusive:int
        ~public_depth:int
        ~private_depth_inclusive:int
        ~private_depth_exclusive:int
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
          ; mining : [ `Inclusive | `Exclusive ]
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
        { private_ = root
        ; public = root
        ; pending_private_to_public_messages = []
        ; mining = `Inclusive
        }
    ;;

    let preferred (BetweenActions state) = state.private_

    let vote_filter = function
      | `Inclusive -> Fun.const true
      | `Exclusive -> N.appended_by_me
    ;;

    let puzzle_payload (BetweenActions state) =
      let vote_filter = vote_filter state.mining in
      N.puzzle_payload' ~vote_filter state.private_
    ;;

    let public_visibility x =
      match visibility x with
      | `Released | `Received -> true
      | `Withheld -> false
    ;;

    let deliver_private_to_public_messages (BetweenActions state) =
      let public =
        List.fold_left
          (fun old consider -> N.update_head ~old (last_block consider))
          state.public
          state.pending_private_to_public_messages
      in
      BeforeAction { public; private_ = state.private_ }
    ;;

    module Dagtools = Dagtools.Make (Block)

    let prepare (BeforeAction state) event =
      let public, private_, event =
        match event with
        | Append _ -> failwith "not implemented"
        | ProofOfWork x ->
          (* work on private chain *)
          state.public, last_block x, `ProofOfWork
        | Network x ->
          (* simulate defender *)
          N.update_head ~old:state.public (last_block x), state.private_, `Network
      in
      let common = Dagtools.common_ancestor public private_ |> Option.get in
      Observable { public; private_; common; event }
    ;;

    let prepare state = deliver_private_to_public_messages state |> prepare

    let observe (Observable state) =
      let open Observation in
      let public_depth, public_votes =
        confirming_votes state.public
        |> BlockSet.filter public_visibility
        |> fun x -> BlockSet.fold (fun x (d, n) -> max d (depth x), n + 1) x (0, 0)
      and private_depth_inclusive, private_votes_inclusive =
        confirming_votes state.private_
        |> fun x -> BlockSet.fold (fun x (d, n) -> max d (depth x), n + 1) x (0, 0)
      and private_depth_exclusive, private_votes_exclusive =
        confirming_votes state.private_
        |> BlockSet.filter N.appended_by_me
        |> fun x -> BlockSet.fold (fun x (d, n) -> max d (depth x), n + 1) x (0, 0)
      in
      let ca_height = height state.common
      and private_height = height state.private_
      and public_height = height state.public in
      { private_blocks = private_height - ca_height
      ; public_blocks = public_height - ca_height
      ; diff_blocks = private_height - public_height
      ; public_votes
      ; private_votes_inclusive
      ; private_votes_exclusive
      ; public_depth
      ; private_depth_inclusive
      ; private_depth_exclusive
      ; event = state.event
      }
    ;;

    let apply (Observable state) action =
      let release kind =
        let rec h release_now seq =
          match seq () with
          | Seq.Nil -> release_now (* override/match not possible; release all *)
          | Seq.Cons (x, seq) ->
            let release_now' = BlockSet.add x release_now in
            let vote_filter x = public_visibility x || BlockSet.mem x release_now' in
            if Block.eq
                 state.public
                 (N.update_head ~vote_filter ~old:state.public (last_block x))
            then (
              (* release_now' is just enough to override; release_now was not enough; *)
              match kind with
              | `Override -> release_now'
              | `Match -> release_now)
            else h release_now' seq
        in
        Dagtools.iterate_descendants ~include_start:true [ state.common ]
        |> Seq.filter (fun x -> not (public_visibility x))
        |> h BlockSet.empty
        |> BlockSet.elements
      in
      let share, private_ =
        match (action : Action.t) with
        | Adopt_Proceed | Adopt_Prolong -> [], state.public
        | Match_Proceed | Match_Prolong -> release `Match, state.private_
        | Override_Proceed | Override_Prolong -> release `Override, state.private_
        | Wait_Proceed | Wait_Prolong -> [], state.private_
      and mining =
        match action with
        | Adopt_Proceed | Override_Proceed | Match_Proceed | Wait_Proceed -> `Inclusive
        | Adopt_Prolong | Override_Prolong | Match_Prolong | Wait_Prolong -> `Exclusive
      in
      BetweenActions
        { public = state.public
        ; private_
        ; pending_private_to_public_messages = share
        ; mining
        }
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
      if o.public_blocks > 0 then Adopt_Proceed else Override_Proceed
    ;;

    let release_block o =
      let open Observation in
      let open Action in
      if o.private_blocks < o.public_blocks
      then Adopt_Proceed
      else if o.private_blocks > o.public_blocks
      then Override_Proceed
      else Wait_Proceed
    ;;

    let override_block o =
      let open Observation in
      let open Action in
      if o.private_blocks < o.public_blocks
      then Adopt_Proceed
      else if o.public_blocks = 0
      then Wait_Proceed
      else Override_Proceed
    ;;

    let override_catchup o =
      let open Observation in
      let open Action in
      if o.private_blocks < o.public_blocks
      then Adopt_Proceed
      else if o.private_blocks = 0 && o.public_blocks = 0
      then Wait_Proceed
      else if o.public_blocks = 0
      then Wait_Proceed
      else if o.private_depth_inclusive = 0 && o.private_blocks = o.public_blocks + 1
      then Override_Proceed
      else if o.public_blocks = o.private_blocks
              && o.private_votes_inclusive = o.public_votes + 1
      then Override_Proceed
      else if o.private_blocks - o.public_blocks > 10
              (* fork can become really deep for strong attackers. Cut-off shortens time
                 spent in common ancestor computation. *)
      then Override_Proceed
      else Wait_Proceed
    ;;

    (* copy & paste from tailstorm_ssz.ml *)
    let minor_delay o =
      let open Observation in
      let open Action in
      if o.public_blocks > o.private_blocks
      then Adopt_Proceed
      else if o.public_blocks = 0
      then Wait_Proceed
      else Override_Proceed
    ;;

    (* copy & paste from tailstorm_ssz.ml *)
    let avoid_loss_alt o =
      let open Observation in
      let open Action in
      let hp = (o.public_blocks * k) + o.public_votes
      and ap = (o.private_blocks * k) + o.private_votes_inclusive in
      match o.public_blocks (* h *), o.private_blocks (* a *) with
      | 0, _ -> Wait_Proceed (* implies h >= 1 for the other branches *)
      | 1, _ when hp = ap -> Match_Proceed
      | _, _ when hp > ap -> Adopt_Proceed
      | _, _ when hp = ap - 1 -> Override_Proceed
      | h, a when h < a - 10 -> Override_Proceed (* cut-off if fork is long *)
      | _, _ -> Wait_Proceed
    ;;
  end

  let policies =
    let open Collection in
    let open Policies in
    empty
    |> add ~info:"emulate honest behaviour" "honest" honest
    |> add ~info:"release private block a.s.a.p." "release-block" release_block
    |> add ~info:"override public block a.s.a.p." "override-block" override_block
    |> add
         ~info:"override public head just before defender catches up"
         "override-catchup"
         override_catchup
    |> add ~info:"override public block a.s.a.p." "minor-delay" minor_delay
    |> add
         ~info:"override public head just before defender catches up"
         "avoid-loss"
         avoid_loss_alt
  ;;
end
