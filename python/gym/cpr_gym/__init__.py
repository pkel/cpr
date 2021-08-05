import gym
import os
from ctypes import PyDLL, RTLD_GLOBAL, c_char_p
import sys

# Link Python/OCaml bridge


try:
    curdir = dir_path = os.path.dirname(os.path.realpath(__file__))
    dll_basename = "bridge.so"
    dll_name = f"{curdir}/{dll_basename}"
    dll_url = "https://pkel.github.io/cpr/bridge.so"
    dll = PyDLL(dll_name, RTLD_GLOBAL)
    argv_t = c_char_p * 2
    argv = argv_t(dll_basename.encode("utf-8"), None)
    dll.caml_startup(argv)
except OSError:
    if not os.path.exists(dll_name):
        print(
            "\n"
            f"{dll_name} not found, please download {dll_basename} "
            f"from {dll_url} and place into {curdir}:\n\n"
            f"curl {dll_url} -o {dll_name}\n\n"
            "you can later update the library with\n\n"
            f"{sys.executable} -m {__name__} --update\n"
        )
        raise ImportError(
            f"{dll_name} not found, please download {dll_basename} "
            f"from {dll_url} and place into {curdir}"
        )
    raise

import engine, specs  # noqa


#  Register gym environment

gym.envs.register(id="cpr-v0", entry_point="cpr_gym.envs:Core")
