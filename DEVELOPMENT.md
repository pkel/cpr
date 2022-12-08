# OCaml

**Opam** is the OCaml package manager. Like pip or npm. We use it do
download and install our OCaml dependencies and to manage different
versions of the OCaml compiler.

You do not have to interact with opam directly. Just make sure that a
recent version (>= 2.0) is installed on your system. [Follow these
instructions](https://opam.ocaml.org/doc/Install.html). Then use `make
setup` to setup an OCaml toolchain in the current working directory
under `./_opam`.

Later, e.g. when dependencies change, run `make dependencies` to update
the toolchain.

**Dune** is an OCaml build system. We use it to build executables and
shared objects, and to run tests. You do not have to interact with dune
directly. Just run `make build` to test whether the build works.

Now, installing `cpr_gym` as editable Python package should work. Try
`pip install -e .`. The editable install will automatically invoke
`dune` to keep the `cpr_gym_engine.so` up to date.

## Ubuntu

```shell
sudo apt install opam
opam init
make setup
make build
pip install -e .
python -m cpr_gym --version
```

## MacOS

```shell
brew install opam
opam init
make setup
make build
pip install -e .
python -m cpr_gym --version
```
