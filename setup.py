import os
import re
import shutil
import subprocess
from setuptools import setup, Extension
from setuptools.command.build_ext import build_ext
from wheel.bdist_wheel import bdist_wheel

try:
    cmd = "git describe --tags --dirty || git describe --all --long --dirty"
    full_version = subprocess.run(
        cmd, shell=True, check=True, stdout=subprocess.PIPE, text=True
    ).stdout.splitlines()[0]

    semver = re.findall("^v[0-9]+.[0-9]+.[0-9]+", full_version)
    if len(semver) > 0:
        version = semver[0]
        if full_version != version:
            version = f"{version}+dev"
    else:
        version = "v0.0.0+notag"

except subprocess.CalledProcessError:
    version = "v0.0.0+nogit"
    full_version = version


class my_bdist_wheel(bdist_wheel):
    def get_tag(self):
        python, abi, plat = super().get_tag()
        # apparently, pyml DLL's are compatible with Python 2 & 3
        # see https://github.com/thierry-martinez/pyml
        # so I guess it's save to set abi = none here
        return "py3", "none", plat


class my_build_ext(build_ext):
    def build_extension(self, ext):
        sources = ext.sources

        if len(sources) == 1 and sources[0].endswith(".ml"):
            ext.source_ml = sources[0]
            self.build_ocaml(ext)
            return

        build_ext.build_extension(self, ext)

    def get_ext_filename(self, name):
        orig = build_ext.get_ext_filename(self, name)
        segs = orig.split(".")
        del segs[-2]
        patched = ".".join(segs)
        return patched

    def build_ocaml(self, ext):
        print(f"my_build_ext: build OCaml extension '{ext.name}'")

        so = ext.source_ml.rsplit(sep=".")[0] + ".so"
        cmd = f"opam exec dune -- build --release {so}"
        print(f"my_build_ext: {cmd}")
        env = os.environ.copy()
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
    ext_modules=[Extension(name="cpr_gym_engine", sources=["simulator/gym/bridge.ml"])],
    cmdclass=dict(bdist_wheel=my_bdist_wheel, build_ext=my_build_ext),
    install_requires=["gym", "numpy"],
)
