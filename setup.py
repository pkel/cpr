import os
import shutil
import subprocess
from setuptools import setup, Extension
from setuptools.command.build_ext import build_ext
from wheel.bdist_wheel import bdist_wheel

with open("VERSION", "r") as f:
    version = f.read().strip()

try:
    cmd = "git describe --tags --dirty || git describe --all --long --dirty"
    full_version = subprocess.run(
        cmd, shell=True, check=True, stdout=subprocess.PIPE, text=True
    ).stdout.splitlines()[0]
    if full_version != version:
        version = f"{version}+dev"
except subprocess.CalledProcessError:
    full_version = version


class my_bdist_wheel(bdist_wheel):
    def get_tag(self):
        python, abi, plat = super().get_tag()
        # apparently, pyml DLL's are compatible with Python 2 & 3
        # see https://github.com/thierry-martinez/pyml
        # so I guess it's save to set abi = none here
        return "py", "none", plat


class my_build_ext(build_ext):
    def build_extension(self, ext):
        sources = ext.sources

        if len(sources) == 1 and sources[0].endswith(".ml"):
            print(f"my_build_ext: extension '{ext.name}' looks like an OCaml extension")
            ext.source_ml = sources[0]
            self.build_ocaml(ext)
            return

        build_ext.build_extension(self, ext)

    def build_ocaml(self, ext):

        so = ext.source_ml.rsplit(sep=".")[0] + ".so"
        cmd = f"opam exec dune -- build --release {so}"
        print(f"my_build_ext: {cmd}")
        env = os.environ
        env["CPR_VERSION"] = full_version
        subprocess.run(cmd, shell=True, check=True, env=env)

        path = self.get_ext_fullpath(ext.name)
        print(f"my_build_ext: copy shared object to {path}")
        shutil.copyfile(f"_build/default/{so}", path)


setup(
    name="cpr_gym",
    version=version,
    packages=["cpr_gym"],
    package_dir={"cpr_gym": "./gym/cpr_gym"},
    ext_modules=[Extension(name="bridge", sources=["simulator/gym/bridge.ml"])],
    cmdclass=dict(bdist_wheel=my_bdist_wheel, build_ext=my_build_ext),
    install_requires=["gym", "numpy"],
)
