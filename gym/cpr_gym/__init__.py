import gym
import os
from ctypes import PyDLL, RTLD_GLOBAL, c_char_p
import sys
import importlib
from . import wrappers  # noqa

# Link Python/OCaml bridge

try:
    # Custom loading for OCaml shared object
    package = "cpr_gym"
    ext_name = "bridge"
    so = "simulator/gym/bridge.so"

    dll_spec = importlib.util.find_spec(ext_name, package)
    if not dll_spec:
        raise ImportError(f"{package}: cannot find DLL for extension '{ext_name}'")

    dll_path = dll_spec.origin
    dll_dir = os.path.dirname(dll_path)
    if os.path.exists(os.path.join(dll_dir, "cpr.opam")):
        if os.path.exists(os.path.join(dll_dir, "_build")):
            print(
                f"{package}: DLL is co-located with OCaml project. Assume editable install."
            )
            print(f"{package}: OCaml build directory (_build) exists. Rebuild DLL.")
            cmd = f"opam exec dune -- build {so}"
            print(f"{package}: {cmd}")
            os.putenv("CPR_VERSION", "local")
            os.system(cmd)
            dll_path = os.path.join(dll_dir, "_build/default", so)

    dll = PyDLL(dll_path, RTLD_GLOBAL)
    argv_t = c_char_p * 2
    argv = argv_t(dll_path.encode("utf-8"), None)
    dll.caml_startup(argv)

    # Make dll-dependent modules available
    import engine, protocols  # noqa

    # Register gym envs
    from . import envs
except OSError:
    if os.path.exists(dll_name):
        raise
    print(
        f"WARNING: missing DLL "
        f"{os.path.relpath(dll_name)}. "
        "The program is likely to fail. Run "
        f"`{os.path.relpath(sys.executable)} -m cpr_gym --update` "
        "to get the latest DLL from Github."
    )
