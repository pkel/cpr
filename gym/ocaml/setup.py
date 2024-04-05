import os
import shutil
import subprocess
from setuptools import setup, Extension
from setuptools.command.build_ext import build_ext
from wheel.bdist_wheel import bdist_wheel
from setuptools_scm import get_version

version = get_version(
    root="../..", git_describe_command="git describe --long --tags --match 'v*'"
)


class cpr_bdist_wheel(bdist_wheel):
    def get_tag(self):
        python, abi, plat = super().get_tag()
        # See discussion in https://github.com/pkel/cpr/pull/31
        if python.startswith("cp"):
            return python, "abi3", plat
        else:
            raise SystemExit("cpr_bdist_wheel: cpython required")


class cpr_build_ext(build_ext):
    def build_extension(self, ext):
        sources = ext.sources

        if len(sources) == 1 and sources[0].endswith(".ml"):
            ext.source_ml = sources[0]
            ext.dune_root = "."
            self.build_ocaml(ext)
            return

        if len(sources) == 2 and sources[1].endswith(".ml"):
            ext.source_ml = sources[1]
            ext.dune_root = sources[0]
            self.build_ocaml(ext)
            return

        build_ext.build_extension(self, ext)

    def get_ext_filename(self, name):
        orig = build_ext.get_ext_filename(self, name)
        segs = orig.split(".")
        del segs[-2]
        patched = ".".join(segs)
        return patched

    def opam_available(self):
        try:
            subprocess.run(["opam", "--version"], capture_output=True)
            return True
        except FileNotFoundError:
            return False
        except subprocess.CalledProcessError:
            return False

    def build_ocaml(self, ext):
        print(f"cpr_build_ext: build OCaml extension '{ext.name}'")

        dest = self.get_ext_fullpath(ext.name)
        if self.opam_available():
            print(f"cpr_build_ext: {ext.source_ml}")
            so = ext.source_ml.removesuffix(".ml") + ".so"
            cmd = f"opam exec dune -- build --profile=release {so}"
            print(f"cpr_build_ext: {cmd}")
            env = os.environ.copy()
            env["CPR_VERSION"] = version
            subprocess.run(cmd, shell=True, check=True, env=env, cwd=ext.dune_root)
            shutil.copyfile(f"{ext.dune_root}/_build/default/{so}", dest)
        else:
            print("cpr_build_ext: OCaml toolchain not available")
            localDLL = "./" + self.get_ext_filename(ext.name)
            if os.path.isfile(localDLL):
                print("cpr_build_ext: reuse existing DLL")
                shutil.copyfile(localDLL, dest)
            else:
                print("cpr_build_ext: no prebuilt DLL found")
                raise SystemExit(f"ERROR: cannot build extension '{ext.name}'")


setup(
    name="cpr_gym",
    version=version,
    description="Gym environment for attacking proof-of-work protocols with RL",
    long_description=open("../../README.md", "r", encoding="utf8").read(),
    long_description_content_type="text/markdown",
    keywords="proof-of-work consensus rl gym selfish-mining reinforcement-learning",
    url="https://github.com/pkel/cpr",
    author="Patrik Keller",
    author_email="git@pkel.dev",
    classifiers=[
        "Development Status :: 4 - Beta",
        "Intended Audience :: Science/Research",
        "Programming Language :: OCaml",
        "Programming Language :: Python :: 3",
        "Topic :: Security",
    ],
    packages=["cpr_gym"],
    package_dir={
        "cpr_gym": "./cpr_gym",
    },
    ext_modules=[
        Extension(
            name="cpr_gym_engine", sources=["../..", "simulator/gym/cpr_gym_engine.ml"]
        )
    ],
    cmdclass=dict(bdist_wheel=cpr_bdist_wheel, build_ext=cpr_build_ext),
    install_requires=[
        "gym<0.22",  # breaking changes ahead; switch to gymnasium 0.27
        "numpy",
    ],
)
