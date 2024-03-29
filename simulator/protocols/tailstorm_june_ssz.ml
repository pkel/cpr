open Cpr_lib

(* This is an attempt to map Tailstorm(ll) as we had it in June, i.e., successful WandB
   run 257 and git commit cc21ff0f3f, to the updated simulator infrastructure.

   Diff to tailstormll_ssz.ml is significant. Maybe keep this as reference even if we drop
   (largely redundant) tailstorm_june.ml. Let's first see, whether we can reproduce WandB
   run 257 against this attack space. *)

module type Parameters = sig
  include Tailstorm_june.Parameters
  include Nakamoto_ssz.Parameters
end

module Make (Parameters : Parameters) = struct
  open Parameters
  module Protocol = Tailstorm_june.Make (Parameters)
  open Protocol

  let () = if unit_observation then failwith "unit_observation=true not implemented"
  let key = Format.asprintf "ssz-%s" (if unit_observation then "unitobs" else "rawobs")

  let info =
    Format.asprintf
      "SSZ'16-like attack space with %s observations"
      (if unit_observation then "unit" else "raw")
  ;;

  module Observation = struct
    type t =
      { public_blocks : int (** number of blocks after common ancestor *)
      ; public_depth : int (** number of votes confirming the leading block *)
      ; private_blocks : int (** number of blocks after common ancestor *)
      ; private_depth : int (** number of votes confirming the leading block *)
      ; diff_blocks : int (** private_blocks - public_blocks *)
      ; diff_depth : int (** private_votes - public_votes *)
      }
    [@@deriving fields]

    let length = List.length Fields.names

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
          ~public_blocks:int
          ~public_depth:int
          ~private_blocks:int
          ~private_depth:int
          ~diff_blocks:int
          ~diff_depth:int
      in
      a
    ;;

    let low =
      to_floatarray
        { public_blocks = 0
        ; public_depth = 0
        ; private_blocks = 0
        ; private_depth = 0
        ; diff_blocks = min_int
        ; diff_depth = min_int
        }
    ;;

    let high =
      to_floatarray
        { public_blocks = max_int
        ; public_depth = max_int
        ; private_blocks = max_int
        ; private_depth = max_int
        ; diff_blocks = max_int
        ; diff_depth = max_int
        }
    ;;

    let of_floatarray =
      let get conv _ i = (fun a -> Float.Array.get a i |> conv), i + 1 in
      let int = get int_of_float in
      fst
        (Fields.make_creator
           0
           ~public_blocks:int
           ~public_depth:int
           ~private_blocks:int
           ~private_depth:int
           ~diff_blocks:int
           ~diff_depth:int)
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
        ~public_blocks:int
        ~public_depth:int
        ~private_blocks:int
        ~private_depth:int
        ~diff_blocks:int
        ~diff_depth:int
      |> String.concat "\n"
    ;;

    let%test _ =
      let run _i =
        let t =
          { public_blocks = Random.bits ()
          ; public_depth = Random.bits ()
          ; private_blocks = Random.bits ()
          ; private_depth = Random.bits ()
          ; diff_blocks = Random.bits ()
          ; diff_depth = Random.bits ()
          }
        in
        t = (to_floatarray t |> of_floatarray)
      in
      List.init 50 run |> List.for_all (fun x -> x)
    ;;
  end

  module Action = Ssz_tools.Action8

  module Agent (V : View with type data = data) = struct
    open Protocol.Referee (V)
    include V
    module Dagtools = Dagtools.Make (Block)

    module State : sig
      open V

      type t = private
        { public : block (* defender's preferred block *)
        ; private_ : block (* attacker's preferred block *)
        ; common : block (* common chain *)
        ; epoch : [ `Proceed | `Prolong ]
              (* Proceed: the attacker considers the defender's votes that extend on his
                 preferred block when building a new block.

                 Prolong: the attacker prolongs the current epoch until he can form a
                 block that does not reference any defender votes. *)
        ; pending_private_to_public_messages : block list
        }

      val init : epoch:[ `Proceed | `Prolong ] -> block -> t

      (* Set fields in state; updates common chain *)
      val update
        :  ?public:block
        -> ?private_:block
        -> ?epoch:[ `Proceed | `Prolong ]
        -> ?pending_private_to_public_messages:block list
        -> t
        -> t
    end = struct
      open V

      type t =
        { public : block
        ; private_ : block
        ; common : block
        ; epoch : [ `Proceed | `Prolong ]
        ; pending_private_to_public_messages : block list
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
        let common = Dagtools.common_ancestor state.public state.private_ in
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

    type state = State.t
    type observable_state = Observable of state

    let preferred (s : state) = last_block s.private_

    let init ~roots =
      let module N = Honest (V) in
      N.init ~roots |> State.init ~epoch:`Prolong
    ;;

    (* the attacker emulates a defending node. This is the local_view of the defender *)

    let public_visibility _state x =
      match visibility x with
      | `Received | `Released -> true
      | `Withheld -> false
    ;;

    let public_view s : (block, data) view =
      (module struct
        include V

        let children x = children x |> List.filter (public_visibility s)
        let parents x = parents x |> List.filter (public_visibility s)

        let data x =
          if not (public_visibility s x) then failwith "invalid access / public view";
          data x
        ;;

        let visibility _x = `Received

        (** The attacker simulates an honest node on the public view. This node should not
           interpret attacker vertices as own vertices. *)
        let my_id = -1
      end)
    ;;

    (* the attacker works on a subset of the total information: he ignores new defender
       blocks *)

    let appended_by_me x =
      match visibility x with
      | `Received -> false
      | `Withheld | `Released -> true
    ;;

    let private_visibility (s : state) vertex =
      (* defender votes for the attacker's preferred block *)
      (* || anything mined by the attacker *)
      (* || anything on the common chain *)
      (s.epoch = `Proceed
      && is_vote vertex
      && Block.eq (last_block vertex) (last_block s.private_))
      || appended_by_me vertex
      || Block.partial_compare s.common vertex >= 0
    ;;

    let private_view (s : state) : _ view =
      (module struct
        include V

        let children x = children x |> List.filter (private_visibility s)
        let parents x = parents x |> List.filter (private_visibility s)
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
      let module N = Honest (Private) in
      N.puzzle_payload (last_block s.private_)
    ;;

    let prepare (state : state) event =
      let state =
        let pending = state.pending_private_to_public_messages in
        List.fold_left
          (fun state msg -> handle_public state (Network msg))
          (State.update ~pending_private_to_public_messages:[] state)
          pending
      in
      match event with
      | Append _ -> failwith "not implemented"
      | ProofOfWork x ->
        assert (private_visibility state x);
        (* work on private chain *)
        handle_private state event
      | Network x ->
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
      let private_depth = (data s.private_).vote
      and public_depth = (data s.public).vote in
      let ca_height = height s.common
      and private_height = height s.private_
      and public_height = height s.public in
      { private_blocks = private_height - ca_height
      ; public_blocks = public_height - ca_height
      ; diff_blocks = private_height - public_height
      ; private_depth
      ; public_depth
      ; diff_depth = private_depth - public_depth
      }
    ;;

    let interpret (s : state) action =
      let parent n =
        match parents n with
        | hd :: _ -> Some hd
        | _ -> None
      in
      let release kind =
        (* find node to be released backwards from private head *)
        let rec h x x' =
          let d = compare_blocks x s.public in
          if d <= 0
          then (
            match kind with
            | `Override -> x'
            | `Match -> x)
          else h (parent x |> Option.get) x
        in
        [ h s.private_ s.private_ ]
        (* NOTE: if private height is smaller public height, then private head is marked
           for release. *)
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

    let conclude (pending_private_to_public_messages, state) =
      { share = pending_private_to_public_messages
      ; state = State.update ~pending_private_to_public_messages state
      ; append = []
      }
    ;;

    let apply (Observable state) action = interpret state action |> conclude
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
      else if o.private_blocks = 0 && o.public_blocks = 0
      then Wait_Proceed
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
      else if o.private_depth = 0 && o.private_blocks = o.public_blocks + 1
      then Override_Proceed
      else if o.public_blocks = o.private_blocks && o.private_depth = o.public_depth + 1
      then Override_Proceed
      else if o.private_blocks - o.public_blocks > 10
              (* fork can become really deep for strong attackers. Cut-off shortens time
                 spent in common ancestor computation. *)
      then Override_Proceed
      else Wait_Proceed
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
  ;;
end
