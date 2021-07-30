import os
from ctypes import PyDLL, RTLD_GLOBAL, c_char_p

curdir = dir_path = os.path.dirname(os.path.realpath(__file__))
dll = PyDLL(f"{curdir}/bridge.so", RTLD_GLOBAL)
argv_t = c_char_p * 2
argv = argv_t("bridge.so".encode('utf-8'), None)
dll.caml_startup(argv)

import engine, environments # noqa
