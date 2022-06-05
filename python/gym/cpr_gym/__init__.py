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
    if sys.platform == "linux":
        dll_url = "https://pkel.github.io/cpr/linux/bridge.so"
    elif sys.platform == "darwin":
        dll_url = "https://pkel.github.io/cpr/macos/bridge.so"
    else:
        raise OSError(f"Unsupported OS: '{sys.platform}' - we support Linux and MacOS")
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

import engine, protocols  # noqa

#  Register gym environments

gym.envs.register(id="core-v0", entry_point="cpr_gym.envs:Core")
gym.envs.register(id="auto-v0", entry_point="cpr_gym.envs:Auto")
