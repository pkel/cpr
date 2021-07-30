#!/bin/bash

set -e

fail () {
  echo "ERROR: pre-commit test failed: $1"
  false
}

make test || fail "\`make test\`"

test "$(ocamlformat --version)" = "0.15.0" || fail "\`ocamlformat --version\` should be 0.15.0"
dune build @fmt || fail "\`dune build @fmt\` (fix with \`make format\`)"
