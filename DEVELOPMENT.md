# OCaml

**Opam** is the OCaml package manager. We use it to get our dependencies
from the internet in the correct version. It also manages different
versions of the OCaml compiler.

You do not have to interact with opam directly. Just make sure that a
recent version (>= 2.0) is installed on your system. Then use `make
setup` to setup an OCaml toolchain in the current working directory
under `./_opam`.

Later, e.g. when dependencies change, run `make dependencies` to update
the toolchain.

**Dune** is an OCaml build system. We use it to build executables and
shared objects, and to run tests. You do not have to interact with dune
directly. Just run `make bridge` to build the latest `bridge.so` and
copy it to the Python `cpr_gym` library.

## Ubuntu

```shell
sudo apt install opam
opam init
make setup
make bridge
```

## MacOS

```shell
brew install opam
opam init
make setup
make bridge
```
