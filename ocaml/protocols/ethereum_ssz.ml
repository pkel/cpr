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
      ; private_orphans : int
            (** number of orphans that could be included as uncles in the attacker chain *)
      ; diff_height : int (** private_height - public_height *)
      ; diff_work : int (** private_work - public_work *)
      ; diff_orphans : int (** private_orphans - public_orphans *)
      ; include_own_orphans : bool
      ; include_foreign_orphans : bool
      ; step_is_attacker_pow : bool
      }
    [@@deriving fields]

    let length = List.length Fields.names

    let low =
      { public_height = 0
      ; public_work = 0
      ; public_orphans = 0
      ; private_height = 0
      ; private_work = 0
      ; private_orphans = 0
      ; diff_height = min_int
      ; diff_work = min_int
      ; diff_orphans = min_int
      ; include_own_orphans = false
      ; include_foreign_orphans = false
      ; step_is_attacker_pow = false
      }
    ;;

    let high =
      { public_height = max_int
      ; public_work = max_int
      ; public_orphans = max_int
      ; private_height = max_int
      ; private_work = max_int
      ; private_orphans = max_int
      ; diff_height = max_int
      ; diff_work = max_int
      ; diff_orphans = max_int
      ; include_own_orphans = true
      ; include_foreign_orphans = true
      ; step_is_attacker_pow = true
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
          ~public_height:int
          ~public_work:int
          ~public_orphans:int
          ~private_height:int
          ~private_work:int
          ~private_orphans:int
          ~diff_height:int
          ~diff_work:int
          ~diff_orphans:int
          ~include_own_orphans:bool
          ~include_foreign_orphans:bool
          ~step_is_attacker_pow:bool
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
           ~public_height:int
           ~public_work:int
           ~public_orphans:int
           ~private_height:int
           ~private_work:int
           ~private_orphans:int
           ~diff_height:int
           ~diff_work:int
           ~diff_orphans:int
           ~include_own_orphans:bool
           ~step_is_attacker_pow:bool
           ~include_foreign_orphans:bool)
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
        ~public_height:int
        ~public_work:int
        ~public_orphans:int
        ~private_height:int
        ~private_work:int
        ~private_orphans:int
        ~diff_height:int
        ~diff_work:int
        ~diff_orphans:int
        ~include_own_orphans:bool
        ~include_foreign_orphans:bool
        ~step_is_attacker_pow:bool
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
          ; private_orphans = Random.bits ()
          ; diff_height = Random.bits ()
          ; diff_work = Random.bits ()
          ; diff_orphans = Random.bits ()
          ; include_own_orphans = Random.bool ()
          ; include_foreign_orphans = Random.bool ()
          ; step_is_attacker_pow = Random.bool ()
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

       3. release some block on the private chain

       . *)
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

    module State : sig
      type t = private
        { public : env Dag.vertex (* defender's preferred block *)
        ; private_ : env Dag.vertex (* attacker's preferred block *)
        ; common : env Dag.vertex (* common chain *)
        ; pending_private_to_public_messages : env Dag.vertex list
        ; mining : Action.uncles
        }

      val init : mining:Action.uncles -> env Dag.vertex -> t

      (* Set fields in state; updates common chain *)
      val update
        :  ?public:env Dag.vertex
        -> ?private_:env Dag.vertex
        -> ?pending_private_to_public_messages:env Dag.vertex list
        -> ?mining:Action.uncles
        -> t
        -> t
    end = struct
      type t =
        { public : env Dag.vertex
        ; private_ : env Dag.vertex
        ; common : env Dag.vertex
        ; pending_private_to_public_messages : env Dag.vertex list
        ; mining : Action.uncles
        }

      let init ~mining x =
        { public = x
        ; private_ = x
        ; common = x
        ; pending_private_to_public_messages = []
        ; mining
        }
      ;;

      (* call this whenever public or private_ changes *)
      let set_common state =
        let common = Dag.common_ancestor view state.public state.private_ in
        assert (Option.is_some common) (* all our protocols maintain this invariant *);
        { state with common = Option.get common }
      ;;

      let update ?public ?private_ ?pending_private_to_public_messages ?mining t =
        set_common
          { public = Option.value ~default:t.public public
          ; private_ = Option.value ~default:t.private_ private_
          ; common = t.common
          ; pending_private_to_public_messages =
              Option.value
                ~default:t.pending_private_to_public_messages
                pending_private_to_public_messages
          ; mining = Option.value ~default:t.mining mining
          }
      ;;
    end

    type state = State.t
    type observable_state = Observable of ([ `PoW | `Deliver ] * state)

    let preferred (s : state) = s.private_

    let init ~roots =
      let module N = Honest (V) in
      N.init ~roots |> State.init ~mining:{ own = true; foreign = true }
    ;;

    (* the attacker emulates a defending node. This is the local_view of the defender *)

    let public_visibility x =
      match visibility x with
      | `Released | `Received -> true
      | `Withheld -> false
    ;;

    module Public_view =
      (val Ssz_tools.emulated_view
             ~pretend_not_me:true
             ~filter:public_visibility
             (module V))

    module Public = Honest (Public_view)

    (* the attacker emulates a defending node. This describes the defender node *)
    let handle_public (s : state) event =
      let public = (Public.handler s.public event).state in
      State.update ~public s
    ;;

    module Private = Honest (V)

    let puzzle_payload (s : state) =
      Private.puzzle_payload'
        ~uncle_filter:(fun x ->
          (s.mining.own && Private.appended_by_me x)
          || (s.mining.foreign && not (Private.appended_by_me x)))
        s.private_
    ;;

    let handle_private (s : state) event =
      let private_ = (Private.handler s.private_ event).state in
      State.update ~private_ s
    ;;

    let prepare (state : state) event =
      let state =
        let pending = state.pending_private_to_public_messages in
        List.fold_left
          (fun state msg -> handle_public state (Network msg))
          (State.update ~pending_private_to_public_messages:[] state)
          pending
      in
      let state =
        match event with
        | Append _ -> failwith "not implemented"
        | ProofOfWork _ ->
          (* work on private chain *)
          `PoW, handle_private state event
        | Network x ->
          let state =
            (* simulate defender *)
            handle_public state event
          in
          (* deliver visible (not ignored) messages *)
          if public_visibility x
          then `Deliver, handle_private state event
          else `Deliver, state
      in
      Observable state
    ;;

    let observe (Observable (ev, s)) =
      let open Observation in
      let private_proposal = Private.puzzle_payload s.private_
      and public_proposal = Public.puzzle_payload s.public in
      let common = data s.common
      and private_ = data s.private_
      and public = data s.public in
      let public_height = public.height - common.height
      and public_work = public.work - common.work
      and public_orphans = List.length public_proposal.parents - 1
      and private_height = private_.height - common.height
      and private_work = private_.work - common.work
      and private_orphans = List.length private_proposal.parents - 1 in
      { public_height
      ; public_work
      ; public_orphans
      ; private_height
      ; private_work
      ; private_orphans
      ; diff_height = private_height - public_height
      ; diff_work = private_work - public_work
      ; diff_orphans = private_orphans - public_orphans
      ; include_own_orphans = s.mining.own
      ; include_foreign_orphans = s.mining.foreign
      ; step_is_attacker_pow =
          (match ev with
          | `PoW -> true
          | `Deliver -> false)
      }
    ;;

    let interpret (s : state) (action, mining) =
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
        [ f s.private_ ]
        (* NOTE: if private preference is smaller target preference, then private head is
           released. *)
      in
      let release, state =
        match (action : Action.action) with
        | Adopt_release -> [ s.private_ ], State.update ~private_:s.public s
        | Adopt_discard -> [], State.update ~private_:s.public s
        | Match -> release_upto (preference (data s.public)), s
        | Override -> release_upto (preference (data s.public) + 1), s
        | Release1 -> release_upto (preference (data s.common) + 1), s
        | Wait -> [], s
      in
      release, State.update ~mining state
    ;;

    let conclude (pending_private_to_public_messages, state) =
      { share = pending_private_to_public_messages
      ; state = State.update ~pending_private_to_public_messages state
      ; append = []
      }
    ;;

    let apply (Observable (_, state)) action = interpret state action |> conclude
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
        if o.step_is_attacker_pow
        then selfish_pool_mines_new_block ()
        else honest_miner_adds_new_block ()
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
        if o.step_is_attacker_pow
        then selfish_pool_mines_new_block ()
        else honest_miner_adds_new_block ()
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
