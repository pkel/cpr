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

let python_of_info i =
  Py.Dict.of_bindings_map
    python_of_string
    (let open Cpr_lib.Info in
    function
    | Bool x -> python_of_bool x
    | Int x -> python_of_int x
    | Float x -> python_of_float x
    | String x -> python_of_string x)
    i
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
     and alpha = keyword "alpha" float ~docstring:"attacker's relative compute"
     and gamma =
       keyword
         "gamma"
         float
         ~docstring:"similar to gamma parameter in selfish mining literature"
     and defenders = keyword "defenders" int ~docstring:"number of defenders"
     and activation_delay =
       keyword
         "activation_delay"
         float
         ~docstring:"expected delay between two consecutive puzzle solutions"
     and max_steps =
       keyword
         "max_steps"
         int
         ~docstring:"maximum number of attacker steps before terminating the simulation"
         ~default:max_int
     and max_progress =
       keyword
         "max_progress"
         float
         ~docstring:"maximum blockchain progress before terminating the simulation"
         ~default:infinity
     and max_time =
       keyword
         "max_time"
         float
         ~docstring:"maximum simulated time before terminating the simulation"
         ~default:infinity
     in
     let (Proto p) = proto in
     let config =
       Engine.Parameters.t
         ~alpha
         ~gamma
         ~defenders
         ~activation_delay
         ~max_steps
         ~max_progress
         ~max_time
     in
     let t = p config in
     fun () -> IEnv (t, t.create ()) |> python_of_ienv);
  Py_module.set
    m
    "reset"
    (let%map ienv = positional "ienv" ienv ~docstring:"OCaml gym environment instance" in
     let (IEnv (t, i)) = ienv in
     fun () -> t.reset i |> Py.Array.numpy);
  Py_module.set
    m
    "step"
    (let%map ienv = positional "ienv" ienv ~docstring:"OCaml gym environment instance"
     and action = positional "action" int ~docstring:"OCaml action" in
     let (IEnv (t, i)) = ienv in
     let obs, r, d, info = t.step i ~action in
     fun () ->
       Py.Tuple.of_array
         [| Py.Array.numpy obs
          ; Py.Float.of_float r
          ; Py.Bool.of_bool d
          ; python_of_info info
         |]);
  Py_module.set
    m
    "policies"
    (let%map (IEnv (t, _)) =
       positional "ienv" ienv ~docstring:"OCaml gym environment instance"
     in
     fun () ->
       List.map
         (fun (name, f) ->
           let defunc =
             let%map obj =
               positional "view" pyobject ~docstring:"observation numpy array"
             in
             fun () ->
               let ba = Numpy.to_bigarray Bigarray.Float64 Bigarray.C_layout obj in
               if Bigarray.Genarray.dims ba <> [| t.observation_length |]
               then raise Py.(Err (ValueError, "invalid dimensions"));
               let arr =
                 Float.Array.init t.observation_length (fun i ->
                     Bigarray.Genarray.get ba [| i |])
               in
               f arr |> python_of_int
           in
           let pyfn args =
             Defunc.apply defunc args (Base.Map.empty (module Base.String))
           in
           name, Py.Callable.of_function pyfn)
         t.policies
       |> Py.Dict.of_bindings_string);
  Py_module.set
    m
    "to_string"
    (let%map ienv = positional "ienv" ienv ~docstring:"OCaml gym environment instance" in
     let (IEnv (t, i)) = ienv in
     fun () -> t.to_string i |> python_of_string);
  Py_module.set
    m
    "n_actions"
    (let%map ienv = positional "ienv" ienv ~docstring:"OCaml gym environment instance" in
     let (IEnv (t, _)) = ienv in
     fun () -> python_of_int t.n_actions);
  Py_module.set
    m
    "observation_low"
    (let%map ienv = positional "ienv" ienv ~docstring:"OCaml gym environment instance" in
     let (IEnv (t, _)) = ienv in
     fun () -> Py.Array.numpy t.low);
  Py_module.set
    m
    "observation_high"
    (let%map ienv = positional "ienv" ienv ~docstring:"OCaml gym environment instance" in
     let (IEnv (t, _)) = ienv in
     fun () -> Py.Array.numpy t.high)
;;

let () =
  let open Cpr_protocols in
  let m = Py_module.create "protocols" in
  Py_module.set
    m
    "nakamoto"
    (let%map unit_observation =
       keyword
         "unit_observation"
         bool
         ~docstring:"Whether to map the observation space to [0,1]^n or not"
     in
     fun () ->
       Proto (Engine.of_module (nakamoto_ssz ~unit_observation)) |> python_of_protocol);
  Py_module.set
    m
    "ethereum"
    (let%map reward = keyword "reward" string ~docstring:"reward function"
     and unit_observation =
       keyword
         "unit_observation"
         bool
         ~docstring:"Whether to map the observation space to [0,1]^n or not"
     in
     let incentive_scheme = Options.of_string_exn Ethereum.incentive_schemes reward in
     fun () ->
       Proto (Engine.of_module (ethereum_ssz ~unit_observation ~incentive_scheme))
       |> python_of_protocol);
  Py_module.set
    m
    "bk"
    (let%map reward = keyword "reward" string ~docstring:"reward function"
     and k = keyword "k" int ~docstring:"puzzles per block"
     and unit_observation =
       keyword
         "unit_observation"
         bool
         ~docstring:"Whether to map the observation space to [0,1]^n or not"
     in
     let incentive_scheme = Options.of_string_exn Bk.incentive_schemes reward in
     fun () ->
       Proto (Engine.of_module (bk_ssz ~unit_observation ~k ~incentive_scheme))
       |> python_of_protocol);
  Py_module.set
    m
    "spar"
    (let%map reward = keyword "reward" string ~docstring:"reward function"
     and k = keyword "k" int ~docstring:"puzzles per block"
     and unit_observation =
       keyword
         "unit_observation"
         bool
         ~docstring:"Whether to map the observation space to [0,1]^n or not"
     in
     let incentive_scheme = Options.of_string_exn Spar.incentive_schemes reward in
     fun () ->
       Proto (Engine.of_module (spar_ssz ~unit_observation ~k ~incentive_scheme))
       |> python_of_protocol);
  Py_module.set
    m
    "stree"
    (let%map reward = keyword "reward" string ~docstring:"reward function"
     and k = keyword "k" int ~docstring:"puzzles per block"
     and subblock_selection =
       keyword "subblock_selection" string ~docstring:"sub-block selection mechanism"
     and unit_observation =
       keyword
         "unit_observation"
         bool
         ~docstring:"Whether to map the observation space to [0,1]^n or not"
     in
     let incentive_scheme = Options.of_string_exn Stree.incentive_schemes reward
     and subblock_selection =
       Options.of_string_exn Stree.subblock_selections subblock_selection
     in
     fun () ->
       Proto
         (Engine.of_module
            (stree_ssz ~unit_observation ~subblock_selection ~incentive_scheme ~k))
       |> python_of_protocol);
  Py_module.set
    m
    "sdag"
    (let%map reward = keyword "reward" string ~docstring:"reward function"
     and k = keyword "k" int ~docstring:"puzzles per block"
     and subblock_selection =
       keyword "subblock_selection" string ~docstring:"sub-block selection mechanism"
     and unit_observation =
       keyword
         "unit_observation"
         bool
         ~docstring:"Whether to map the observation space to [0,1]^n or not"
     in
     let incentive_scheme = Options.of_string_exn Sdag.incentive_schemes reward
     and subblock_selection =
       Options.of_string_exn Sdag.subblock_selections subblock_selection
     in
     fun () ->
       Proto
         (Engine.of_module
            (sdag_ssz ~unit_observation ~subblock_selection ~incentive_scheme ~k))
       |> python_of_protocol);
  Py_module.set
    m
    "tailstorm"
    (let%map reward = keyword "reward" string ~docstring:"reward function"
     and k = keyword "k" int ~docstring:"puzzles per block"
     and subblock_selection =
       keyword "subblock_selection" string ~docstring:"sub-block selection mechanism"
     and unit_observation =
       keyword
         "unit_observation"
         bool
         ~docstring:"Whether to map the observation space to [0,1]^n or not"
     in
     let incentive_scheme = Options.of_string_exn Tailstorm.incentive_schemes reward
     and subblock_selection =
       Options.of_string_exn Tailstorm.subblock_selections subblock_selection
     in
     fun () ->
       Proto
         (Engine.of_module
            (tailstorm_ssz ~unit_observation ~subblock_selection ~incentive_scheme ~k))
       |> python_of_protocol);
  Py_module.set
    m
    "tailstormjune"
    (let%map reward = keyword "reward" string ~docstring:"reward function"
     and k = keyword "k" int ~docstring:"puzzles per block"
     and unit_observation =
       keyword
         "unit_observation"
         bool
         ~docstring:"Whether to map the observation space to [0,1]^n or not"
     in
     let incentive_scheme = Options.of_string_exn Tailstorm.incentive_schemes reward in
     fun () ->
       Proto (Engine.of_module (tailstormjune_ssz ~unit_observation ~incentive_scheme ~k))
       |> python_of_protocol)
;;
