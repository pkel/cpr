open Cpr_lib
open Models

let task =
  { network = TwoAgentsZero { alpha = 0.33 }
  ; incentive_schemes = [ Constant ]
  ; protocol = B_k_lessleadership { k = 16 }
  ; scenario = FirstSelfish
  ; strategy = SelfishSimple
  ; activations = 200
  ; activation_delay = 1.
  }
;;

let run task =
  let (S s) = setup task in
  (* simulate *)
  let open Simulator in
  init ?deviations:s.deviations s.params s.protocol
  |> loop s.params
  |> fun sim ->
  Dag.dot
    stdout
    sim.global_view
    (fun n ->
      let d = Dag.data n in
      Printf.sprintf
        "%s | %.2f%s"
        (match d.appended_by with
        | None -> "root"
        | Some 0 -> "a"
        | Some _ -> "d")
        d.appended_at
        (match Dag.children sim.global_view n with
        | [] -> " | o"
        | _ -> ""))
    (Dag.roots sim.dag)
;;

let () = run task
