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

type packed_env = PEnv : ('instance, 'observation) env -> packed_env

type instantiated_env =
  | IEnv : ('instance, 'observation) env * 'instance -> instantiated_env

let (ienv, python_of_ienv, _python_to_ienv) =
  (encapsulate "ocaml.instantiated_env" : instantiated_env capsule)
;;

let (penv, python_of_penv, _python_to_penv) =
  (encapsulate "ocaml.packed_env" : packed_env capsule)
;;

let () =
  if not (Py.is_initialized ()) then Py.initialize ();
  Random.self_init ();
  let m = Py_module.create "engine" in
  Py_module.set
    m
    "create"
    (let%map penv = positional "env" penv ~docstring:"OCaml gym environment" in
     let (PEnv t) = penv in
     IEnv (t, t.create ()) |> python_of_ienv);
  Py_module.set
    m
    "reset"
    (let%map ienv = positional "ienv" ienv ~docstring:"OCaml gym environment instance" in
     let (IEnv (t, i)) = ienv in
     t.reset i |> t.numpy |> Py.Array.numpy);
  Py_module.set
    m
    "step"
    (let%map ienv = positional "ienv" ienv ~docstring:"OCaml gym environment instance"
     and action = positional "action" int ~docstring:"OCaml action" in
     let (IEnv (t, i)) = ienv in
     let v, r, d = t.step i ~action in
     Py.Tuple.of_array
       [| t.numpy v |> Py.Array.numpy; Py.Float.of_float r; Py.Bool.of_bool d |]);
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
  let m = Py_module.create "specs" in
  Py_module.set_value m "default" (PEnv default |> python_of_penv);
  Py_module.set
    m
    "bk"
    (let%map k = keyword "k" int ~docstring:"number of votes per block"
     and alpha = keyword "alpha" float ~docstring:"attacker's relative compute" in
     PEnv (bk ~k ~alpha) |> python_of_penv)
;;
