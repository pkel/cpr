import importlib.metadata
import importlib.resources
import os
import pathlib
from ctypes import PyDLL, RTLD_GLOBAL, c_char_p
import sys

# Derive version from github repo
#################################

package = "cpr_gym"
ext_name = "cpr_gym_engine"
ext_build_name = "simulator/gym/cpr_gym_engine.so"

__version__ = "v" + importlib.metadata.version(package)
__repo_path = pathlib.Path(__file__).parent.parent.parent

editable_install = False
if __repo_path.joinpath("cpr.opam").is_file():
    print(f"{package}: Editable install detected.")
    editable_install = True
    repo_path = __repo_path

    import git

    repo = git.Repo(repo_path)
    try:
        commit = repo.git.describe(tags=True, dirty=True)
    except git.exc.GitCommandError:
        commit = repo.git.describe(all=True, long=True, dirty=True)
    __version__ = commit

# Custom loading for OCaml shared object
########################################

# Find DLL
# -------

dll_spec = importlib.util.find_spec(ext_name, package)
if not dll_spec:
    raise ImportError(f"{package}: cannot find DLL for extension '{ext_name}'")

dll_path = dll_spec.origin
dll_dir = os.path.dirname(dll_path)

# Rebuild DLL locally?
# --------------------

if editable_install:
    if os.path.exists(os.path.join(repo_path, "_build")):
        cmd = f"opam exec dune -- build {ext_build_name}"
        print(
            f"{package}: OCaml build directory (_build) exists. Rebuild DLL.\n"
            f"{package}: {cmd}"
        )
        os.putenv("CPR_VERSION", __version__)
        import subprocess

        subprocess.run(cmd, shell=True, check=True, cwd=repo_path)
        dll_path = os.path.join(dll_dir, "_build/default", ext_build_name)

# Custom import
# -------------

dll = PyDLL(dll_path, RTLD_GLOBAL)
argv_t = c_char_p * 2
argv = argv_t(dll_path.encode("utf-8"), None)
dll.caml_startup(argv)

# Extension available
# -------------------

# Make dll-dependent modules available
import engine, protocols  # noqa

engine_version = engine.cpr_lib_version.strip()
if __version__ != engine_version:
    __version__ = f"{__version__} (engine: {engine_version})"

# Register gym envs
from . import envs

# Make available other modules
from . import wrappers
