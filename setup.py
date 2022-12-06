from setuptools import setup, Extension
from setuptools.command.build_ext import build_ext


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
        import os
        import shutil

        so = ext.source_ml.rsplit(sep=".")[0] + ".so"
        cmd = f"opam exec dune -- build --release {so}"
        print(f"my_build_ext: {cmd}")
        os.system(cmd)

        path = self.get_ext_fullpath(ext.name)
        print(f"my_build_ext: copy shared object to {path}")
        shutil.copyfile(f"_build/default/{so}", path)


setup(
    name="cpr_gym",
    packages=["cpr_gym"],
    package_dir={"cpr_gym": "./gym/cpr_gym"},
    ext_modules=[Extension(name="bridge", sources=["simulator/gym/bridge.ml"])],
    cmdclass=dict(build_ext=my_build_ext),
    install_requires=["gym", "numpy"],
)
