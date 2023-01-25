#!/usr/bin/env bash

set -Eeuo pipefail

repo=$(git -C "$(dirname "$(realpath "$0")")" rev-parse --show-toplevel)

if [ $# -lt 1 ] ; then
  echo provide python version number as argument >&2
  exit 1
fi

# if virtual env is setup, stick to it
if [ -x "$repo/_venv/bin/python" ] ; then
  realpath -s --relative-to="$(pwd)" "$repo/_venv/bin/python"
  exit 0
fi

# if preferred versions available, use them
for v in "${@}" ; do
  if which "python$v" > /dev/null 2>&1 ; then
    echo "python$v"
    exit 0
  fi
done

# fallback to python3
if which python3 > /dev/null 2>&1 ; then
  for v in "${@}" ; do
    if python3 --version | grep -F "$v" > /dev/null ; then
      echo python3
      exit 0
    fi
  done
  echo incompatible python3 --version >&2
  exit 1
fi

echo python3 not found >&2
exit 1
