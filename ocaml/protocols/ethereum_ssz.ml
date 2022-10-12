open Cpr_lib

module Make (Parameters : Ethereum.Parameters) = struct
  module Protocol = Ethereum.Make (Parameters)
  open Protocol

  let key = "ssz"
  let info = "SSZ'16-like attack space"

  module Observation = struct
    type t =
      { public_height : int
            (** defender chain, number of sequential blocks after common ancestor *)
      ; public_work : int
            (** defender chain, number of blocks after common ancestor, including uncles *)
      ; public_orphans : int
            (** number of orphans that could be included as uncles in the public chain *)
      ; private_height : int
            (** attacker chain, number of sequential blocks after common ancestor *)
      ; private_work : int
            (** attacker chain, number of blocks after common ancestor, including uncles *)
      ; private_orphans_inclusive : int
            (** number of orphans that could be included as uncles in the attacker chain; including defender orphans *)
      ; private_orphans_exclusive : int
            (** number of orphans that could be included as uncles in the attacker chain; excluding defender orphans *)
      ; diff_height : int (** private_height - public_height *)
      ; diff_work : int (** private_work - public_work *)
      ; event : int
      }
    [@@deriving fields]

    let length = List.length Fields.names

    let low =
      { public_height = 0
      ; public_work = 0
      ; public_orphans = 0
      ; private_height = 0
      ; private_work = 0
      ; private_orphans_inclusive = 0
      ; private_orphans_exclusive = 0
      ; diff_height = min_int
      ; diff_work = min_int
      ; event = Ssz_tools.Event.low
      }
    ;;

    let high =
      { public_height = max_int
      ; public_work = max_int
      ; public_orphans = max_int
      ; private_height = max_int
      ; private_work = max_int
      ; private_orphans_inclusive = max_int
      ; private_orphans_exclusive = max_int
      ; diff_height = max_int
      ; diff_work = max_int
      ; event = Ssz_tools.Event.high
      }
    ;;

    let to_floatarray t =
      let a = Float.Array.make length Float.nan in
      let set conv i field =
        Float.Array.set a i (Fieldslib.Field.get field t |> conv);
        i + 1
      in
      let int = set float_of_int in
      let _ =
        Fields.fold
          ~init:0
          ~public_height:int
          ~public_work:int
          ~public_orphans:int
          ~private_height:int
          ~private_work:int
          ~private_orphans_inclusive:int
          ~private_orphans_exclusive:int
          ~diff_height:int
          ~diff_work:int
          ~event:int
      in
      a
    ;;

    let of_floatarray =
      let get conv _ i = (fun a -> Float.Array.get a i |> conv), i + 1 in
      let int = get int_of_float in
      fst
        (Fields.make_creator
           0
           ~public_height:int
           ~public_work:int
           ~public_orphans:int
           ~private_height:int
           ~private_work:int
           ~private_orphans_inclusive:int
           ~private_orphans_exclusive:int
           ~diff_height:int
           ~diff_work:int
           ~event:int)
    ;;

    let to_string t =
      let conv to_s field =
        Printf.sprintf
          "%s: %s"
          (Fieldslib.Field.name field)
          (to_s (Fieldslib.Field.get field t))
      in
      let int = conv string_of_int in
      Fields.to_list
        ~public_height:int
        ~public_work:int
        ~public_orphans:int
        ~private_height:int
        ~private_work:int
        ~private_orphans_inclusive:int
        ~private_orphans_exclusive:int
        ~diff_height:int
        ~diff_work:int
        ~event:int
      |> String.concat "\n"
    ;;

    let%test _ =
      let run _i =
        let t =
          { public_height = Random.bits ()
          ; public_work = Random.bits ()
          ; public_orphans = Random.bits ()
          ; private_height = Random.bits ()
          ; private_work = Random.bits ()
          ; private_orphans_inclusive = Random.bits ()
          ; private_orphans_exclusive = Random.bits ()
          ; diff_height = Random.bits ()
          ; diff_work = Random.bits ()
          ; event = Random.bits ()
          }
        in
        t = (to_floatarray t |> of_floatarray)
      in
      List.init 50 run |> List.for_all (fun x -> x)
    ;;
  end

  module Action = struct
    (* Bitcoin has Adopt, Override, Match, and Wait Actions. Ethereum adds a couple of
       choices:

       1. attacker can release own blocks on adopt (or not) with the intention that the
       defender adopts it as uncle.

       2. attacker can include foreign orphans or ignore them

       3. release some block on the private chain with the intention that the defender
       adopts it as uncle. *)
    type action =
      | Adopt_discard
          (** Adopt the defender's preferred chain as attacker's preferred chain. Withheld
              attacker blocks are discarded.

              Equivalent to Adopt in SSZ'16 model. *)
      | Adopt_release
          (** Adopt the defender's preferred chain as attacker's preferred chain. Withheld
              attacker blocks are released.

              No equivalent in SSZ'16 model. *)
      | Override
          (** Publish just enough information to make the defender adopt the chain just
              released. The attacker continues mining the private chain.

              If override is impossible, this still results in a release of withheld
              information.

              Equivalent to Override in SSZ'16 model. *)
      | Match
          (** Publish just enough information such that the defender observes a tie
              between two chains. The attacker continues mining the private chain.

              If match is impossible, this still results in a release of withheld
              information.

              Equivalent to Match in SSZ'16 model. *)
      | Release1
          (** Publish one block from the private chain, continue mining on the private chain.

              No equivalent in SSZ'16. *)
      | Wait
          (** Continue withholding. Always possible. Equivalent to Wait in SSZ'16 model. *)
    [@@deriving variants]

    let action_to_string = Variants_of_action.to_name
    let action_to_int = Variants_of_action.to_rank

    let action_list =
      let add acc var = var.Variantslib.Variant.constructor :: acc in
      Variants_of_action.fold
        ~init:[]
        ~adopt_discard:add
        ~adopt_release:add
        ~override:add
        ~match_:add
        ~release1:add
        ~wait:add
      |> List.rev
    ;;

    (* Set mining rule. Which uncles should be included? *)
    type uncles =
      { own : bool
      ; foreign : bool
      }
    [@@deriving fields]

    let uncles_to_string { own; foreign } =
      let string_of_bool b = if b then "true" else "false" in
      Printf.sprintf
        "uncles {own: %s; foreign: %s}"
        (string_of_bool own)
        (string_of_bool foreign)
    ;;

    let uncles_list =
      let bools = [ false; true ] in
      List.concat_map (fun own -> List.map (fun foreign -> { own; foreign }) bools) bools
    ;;

    type t = action * uncles

    let to_string (action, uncles) =
      action_to_string action ^ ", " ^ uncles_to_string uncles
    ;;

    let table =
      List.concat_map
        (fun action -> List.map (fun uncles -> action, uncles) uncles_list)
        action_list
      |> Array.of_list
    ;;

    let of_int i = table.(i)
    let n = Array.length table

    let to_int =
      let ht = Hashtbl.create n in
      let () = Array.iteri (fun i x -> Hashtbl.add ht x i) table in
      Hashtbl.find ht
    ;;

    let%test_unit "to_int" =
      let hit = Array.make n false in
      let () = Array.iter (fun a -> hit.(to_int a) <- true) table in
      assert (Array.for_all (fun x -> x) hit)
    ;;

    let%test_unit "of_int" =
      for i = 0 to n - 1 do
        let a = of_int i in
        assert (i == to_int a)
      done
    ;;
  end

  module Agent (V : LocalView with type data = data) = struct
    open Protocol.Referee (V)
    include V
    module N = Honest (V)

    type state =
      | BetweenActions of
          { public : env Dag.vertex (* defender's preferred block *)
          ; private_ : env Dag.vertex (* attacker's preferred block *)
          ; mining : Action.uncles (* mining rule, which uncles to include *)
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
          ; event : [ `ProofOfWork | `Network ]
          }

    let init ~roots =
      let root = N.init ~roots in
      BetweenActions
        { private_ = root
        ; public = root
        ; pending_private_to_public_messages = []
        ; mining = { own = true; foreign = true }
        }
    ;;

    let preferred (BetweenActions state) = state.private_

    let uncle_filter mining x =
      let open Action in
      (mining.own && N.appended_by_me x) || (mining.foreign && not (N.appended_by_me x))
    ;;

    let puzzle_payload (BetweenActions state) =
      let uncle_filter = uncle_filter state.mining in
      N.puzzle_payload' ~uncle_filter state.private_
    ;;

    let public_visibility x =
      match visibility x with
      | `Released | `Received -> true
      | `Withheld -> false
    ;;

    let deliver_private_to_public_messages (BetweenActions state) =
      let public =
        List.fold_left
          (fun old consider -> N.update_head ~old consider)
          state.public
          state.pending_private_to_public_messages
      in
      BeforeAction { public; private_ = state.private_ }
    ;;

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
      let common = Dag.common_ancestor view public private_ |> Option.get in
      Observable { public; private_; common; event }
    ;;

    let prepare state = deliver_private_to_public_messages state |> prepare

    let observe (Observable state) =
      let open Observation in
      let common = data state.common
      and private_ = data state.private_
      and public = data state.public in
      let public_height = public.height - common.height
      and public_work = public.work - common.work
      and public_orphans =
        let draft = N.puzzle_payload' ~uncle_filter:public_visibility state.public in
        List.length draft.parents - 1
      and private_height = private_.height - common.height
      and private_work = private_.work - common.work
      and private_orphans_inclusive =
        let uncle_filter = uncle_filter { own = true; foreign = true } in
        let draft = N.puzzle_payload' ~uncle_filter state.private_ in
        List.length draft.parents - 1
      and private_orphans_exclusive =
        let uncle_filter = uncle_filter { own = true; foreign = false } in
        let draft = N.puzzle_payload' ~uncle_filter state.private_ in
        List.length draft.parents - 1
      in
      { public_height
      ; public_work
      ; public_orphans
      ; private_height
      ; private_work
      ; private_orphans_inclusive
      ; private_orphans_exclusive
      ; diff_height = private_height - public_height
      ; diff_work = private_work - public_work
      ; event = Ssz_tools.Event.to_int state.event
      }
    ;;

    let apply (Observable state) (action, mining) =
      let parent vtx =
        match Dag.parents view vtx with
        | hd :: _tl -> Some hd
        | _ -> None
      in
      let release_upto target =
        (* look for to be released block backwards from private head *)
        let rec f b =
          if preference (data b) <= target then b else parent b |> Option.get |> f
        in
        [ f state.private_ ]
        (* NOTE: if private preference is smaller target preference, then private head is
           released. *)
      in
      let share, private_ =
        match (action : Action.action) with
        | Adopt_release -> [ state.private_ ], state.public
        | Adopt_discard -> [], state.public
        | Match -> release_upto (preference (data state.public)), state.private_
        | Override -> release_upto (preference (data state.public) + 1), state.private_
        | Release1 -> release_upto (preference (data state.common) + 1), state.private_
        | Wait -> [], state.private_
      in
      BetweenActions
        { private_
        ; public = state.public
        ; pending_private_to_public_messages = share
        ; mining
        }
      |> return ~share
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
      let mining = { own = true; foreign = true } in
      if o.public_work > 0 then Adopt_release, mining else Override, mining
    ;;

    let selfish ?(adopt = `Release) o =
      (* Ad-hoc strategy. This is probably not optimal. *)
      let open Observation in
      let open Action in
      let mining = { own = true; foreign = false } in
      let adopt =
        match adopt with
        | `Release -> Adopt_release
        | `Discard -> Adopt_discard
      and private_pref, public_pref =
        match Parameters.preference with
        | Height -> o.private_height, o.public_height
        | Work -> o.private_work, o.public_work
      in
      if private_pref < public_pref
      then adopt, mining
      else if private_pref = 0 && public_pref = 0
      then Wait, mining
      else if public_pref = 0
      then Wait, mining
      else Override, mining
    ;;

    (** Feng and Niu. Selfish mining in Ethereum. ICDCS '19. *)
    let fn19 o =
      let open Observation in
      let open Action in
      let selfish_pool_mines_new_block () =
        if o.private_height = 2 && o.public_height = 1 then Override else Wait
      and honest_miner_adds_new_block () =
        if o.private_height < o.public_height
        then Adopt_discard
        else if o.private_height = o.public_height
        then Match
        else if o.private_height = o.public_height + 1
        then Override
        else Release1
      in
      let a =
        match Ssz_tools.Event.of_int o.event with
        | `Append -> failwith "not implemented"
        | `ProofOfWork -> selfish_pool_mines_new_block ()
        | `Network -> honest_miner_adds_new_block ()
      in
      (* I think the strategy could be improved by ignoring honest uncles and by using
         Adopt_release. *)
      a, { own = true; foreign = true }
    ;;

    let fn19pkel o =
      let open Observation in
      let open Action in
      let selfish_pool_mines_new_block () =
        if o.private_height = 2 && o.public_height = 1 then Override else Wait
      and honest_miner_adds_new_block () =
        if o.private_height < o.public_height
        then Adopt_release
        else if o.private_height = o.public_height
        then Match
        else if o.private_height = o.public_height + 1
        then Override
        else Release1
      in
      let a =
        match Ssz_tools.Event.of_int o.event with
        | `Append -> failwith "not implemented"
        | `ProofOfWork -> selfish_pool_mines_new_block ()
        | `Network -> honest_miner_adds_new_block ()
      in
      a, { own = true; foreign = false }
    ;;
  end

  let policies =
    let open Collection in
    let open Policies in
    empty
    |> add ~info:"emulate honest behaviour" "honest" honest
    |> add
         ~info:"ad-hoc selfish policy w/ release on adopt"
         "selfish_release"
         (selfish ~adopt:`Release)
    |> add
         ~info:"ad-hoc selfish policy w/ discard on adopt"
         "selfish_discard"
         (selfish ~adopt:`Discard)
    |> add ~info:"Feng and Niu. Selfish mining in Ethereum. ICDCS '19." "fn19" fn19
    |> add ~info:"Improved? version of Feng and Niu @ ICDCS '19." "fn19pkel" fn19pkel
  ;;
end
