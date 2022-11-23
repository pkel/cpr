import gym
import os
from ctypes import PyDLL, RTLD_GLOBAL, c_char_p
import sys
from . import wrappers  # noqa

# Link Python/OCaml bridge

try:
    curdir = dir_path = os.path.dirname(os.path.realpath(__file__))
    dll_basename = "bridge.so"
    dll_name = f"{curdir}/{dll_basename}"

    # Load DLL
    dll = PyDLL(dll_name, RTLD_GLOBAL)
    argv_t = c_char_p * 2
    argv = argv_t(dll_basename.encode("utf-8"), None)
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
