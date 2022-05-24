open Intf
open Python_lib
open Python_lib.Let_syntax

type 'a capsule = 'a Defunc.Of_python.t * ('a -> pyobject) * (pyobject -> 'a)

let encapsulate : type a. string -> a capsule =
 fun name ->
  let b, c = Py.Capsule.make name in
  let a = Defunc.Of_python.create ~type_name:name ~conv:c in
  a, b, c
;;

type protocol = Proto : (Engine.Parameters.t -> 'instance env) -> protocol
type instantiated_env = IEnv : 'instance env * 'instance -> instantiated_env

let (ienv, python_of_ienv, _python_to_ienv) =
  (encapsulate "ocaml.instantiated_env" : instantiated_env capsule)
;;

let (penv, python_of_protocol, _python_to_protocol) =
  (encapsulate "ocaml.protocol" : protocol capsule)
;;

let () =
  if not (Py.is_initialized ()) then Py.initialize ();
  Random.self_init ();
  let m = Py_module.create "engine" in
  Py_module.set_value m "cpr_lib_version" (Py.String.of_string Cpr_lib.version);
  Py_module.set
    m
    "create"
    (let%map proto = keyword "proto" penv ~docstring:"OCaml gym protocol spec"
     and alpha =
       keyword "alpha" float ~docstring:"attacker's relative compute" ~default:0.25
     and gamma =
       keyword
         "gamma"
         float
         ~docstring:"similar to gamma parameter in selfish mining literature"
         ~default:0.5
     and defenders = keyword "defenders" int ~docstring:"number of defenders" ~default:2
     and activation_delay =
       keyword
         "activation_delay"
         float
         ~docstring:"expected delay between two consecutive puzzle solutions"
         ~default:1.
     and max_steps =
       keyword
         "max_steps"
         int
         ~docstring:"maximum number of attacker steps before terminating the simulation"
         ~default:1000
     and max_time =
       keyword
         "max_time"
         float
         ~docstring:"maximum simulated time before terminating the simulation"
         ~default:Float.infinity
     in
     let (Proto p) = proto in
     let config =
       Engine.Parameters.t ~alpha ~gamma ~defenders ~activation_delay ~max_steps ~max_time
     in
     let t = p config in
     IEnv (t, t.create ()) |> python_of_ienv);
  Py_module.set
    m
    "reset"
    (let%map ienv = positional "ienv" ienv ~docstring:"OCaml gym environment instance" in
     let (IEnv (t, i)) = ienv in
     t.reset i |> Py.Array.numpy);
  Py_module.set
    m
    "step"
    (let%map ienv = positional "ienv" ienv ~docstring:"OCaml gym environment instance"
     and action = positional "action" int ~docstring:"OCaml action" in
     let (IEnv (t, i)) = ienv in
     let obs, r, d, info = t.step i ~action in
     Py.Tuple.of_array
       [| Py.Array.numpy obs
        ; Py.Float.of_float r
        ; Py.Bool.of_bool d
        ; Py.Dict.of_bindings_string info
       |]);
  Py_module.set
    m
    "policies"
    (let%map (IEnv (t, _)) =
       positional "ienv" ienv ~docstring:"OCaml gym environment instance"
     in
     List.map
       (fun (name, f) ->
         let defunc =
           let%map obj =
             positional "view" pyobject ~docstring:"observation numpy array"
           in
           let ba = Numpy.to_bigarray Bigarray.Float64 Bigarray.C_layout obj in
           if Bigarray.Genarray.dims ba <> [| t.observation_length |]
           then failwith "invalid dimensions";
           let arr =
             Float.Array.init t.observation_length (fun i ->
                 Bigarray.Genarray.get ba [| i |])
           in
           f arr |> python_of_int
         in
         let pyfn args = Defunc.apply defunc args (Base.Map.empty (module Base.String)) in
         name, Py.Callable.of_function pyfn)
       t.policies
     |> Py.Dict.of_bindings_string);
  Py_module.set
    m
    "to_string"
    (let%map ienv = positional "ienv" ienv ~docstring:"OCaml gym environment instance" in
     let (IEnv (t, i)) = ienv in
     t.to_string i |> python_of_string);
  Py_module.set
    m
    "n_actions"
    (let%map ienv = positional "ienv" ienv ~docstring:"OCaml gym environment instance" in
     let (IEnv (t, _)) = ienv in
     python_of_int t.n_actions);
  Py_module.set
    m
    "observation_low"
    (let%map ienv = positional "ienv" ienv ~docstring:"OCaml gym environment instance" in
     let (IEnv (t, _)) = ienv in
     Py.Array.numpy t.low);
  Py_module.set
    m
    "observation_high"
    (let%map ienv = positional "ienv" ienv ~docstring:"OCaml gym environment instance" in
     let (IEnv (t, _)) = ienv in
     Py.Array.numpy t.high)
;;

let () =
  let open Definitions in
  let m = Py_module.create "protocols" in
  Py_module.set
    m
    "nakamoto"
    (let%map reward =
       keyword
         "reward"
         string
         ~default:"block"
         ~docstring:"Select reward function.\n'block': for 1 per confirmed block"
     in
     let reward =
       match reward with
       | "block" -> Cpr_protocols.Nakamoto.constant 1.
       | _ ->
         let msg = "unknown reward function '" ^ reward ^ "'" in
         failwith msg
     in
     Proto (nakamoto ~reward) |> python_of_protocol);
  Py_module.set
    m
    "bk"
    (let%map k = keyword "k" int ~docstring:"number of votes per block"
     and reward =
       keyword
         "reward"
         string
         ~default:"pow"
         ~docstring:
           "Select reward function.\n\
            'pow': for 1 per confirmed proof-of-work vote\n\
            'block': for 1 per confirmed block"
     in
     let reward =
       match reward with
       | "pow" -> Cpr_protocols.B_k.constant_pow 1.
       | "block" -> Cpr_protocols.B_k.constant_block 1.
       | _ ->
         let msg = "unknown reward function '" ^ reward ^ "'" in
         failwith msg
     in
     Proto (bk ~k ~reward) |> python_of_protocol);
  Py_module.set
    m
    "bk_ll"
    (let%map k = keyword "k" int ~docstring:"number of votes per block"
     and reward =
       keyword
         "reward"
         string
         ~default:"pow"
         ~docstring:
           "Select reward function.\n\
            'pow': for 1 per confirmed proof-of-work vote\n\
            'block': for 1 per confirmed block"
     in
     let reward =
       match reward with
       | "pow" -> Cpr_protocols.B_k_lessleader.constant_pow 1.
       | "block" -> Cpr_protocols.B_k_lessleader.constant_block 1.
       | _ ->
         let msg = "unknown reward function '" ^ reward ^ "'" in
         failwith msg
     in
     Proto (bk_ll ~k ~reward) |> python_of_protocol);
  Py_module.set
    m
    "george"
    (let%map k = keyword "k" int ~docstring:"number of votes per block"
     and reward =
       keyword
         "reward"
         string
         ~default:"constant"
         ~docstring:
           "Select reward function.\n\
            'block': k per confirmed block\n\
            'constant': 1 per confirmed pow solution\n\
            'punish': max k per confirmed block, 1 per pow solution on longest chain of \
            votes\n\
            'discount': max k per confirmed block, d/k per pow solution (d ∊ 1..k = \
            height since last block)\n\
            'hybrid': max k per confirmed block, d/k per pow solution on longest chain \
            of votes (d ∊ 1..k = height since last block)"
     in
     let reward =
       match reward with
       | "block" -> Cpr_protocols.George.constant_block (float_of_int k)
       | "constant" ->
         Cpr_protocols.George.reward
           ~max_reward_per_block:1.
           ~punish:false
           ~discount:false
           ~k
       | "punish" ->
         Cpr_protocols.George.reward
           ~max_reward_per_block:1.
           ~punish:true
           ~discount:false
           ~k
       | "discount" ->
         Cpr_protocols.George.reward
           ~max_reward_per_block:1.
           ~punish:false
           ~discount:true
           ~k
       | "hybrid" ->
         Cpr_protocols.George.reward
           ~max_reward_per_block:1.
           ~punish:true
           ~discount:true
           ~k
       | _ ->
         let msg = "unknown reward function '" ^ reward ^ "'" in
         failwith msg
     in
     Proto (george ~k ~reward) |> python_of_protocol)
;;
