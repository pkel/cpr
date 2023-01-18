#!/usr/bin/env bash

set -Eeuo pipefail

repo=$(git rev-parse --show-toplevel)
dst=$repo/data/models

if [ $# -lt 1 ] ; then
  echo not enough arguments
  exit 1
fi

if [ ! -d "$1" ] ; then
  echo Result directory "$1" does not exist. Abort.
  exit 1
fi

name=$(basename "$1")

if [ ! -d "$dst" ] ; then
  echo Target directory "$dst" does not exist. Abort.
  exit 2
fi

if [ -e "$dst/$name" ] ; then
  echo Target directory "$dst/$name" already exists. Abort.
  exit 3
fi

for z in "$1"/*.zip ; do
  unzip "$z" -d "$1"
done

mkdir "$dst/$name"

for m in "$1"/saved_models/*/best_model.zip ; do
  x=$(basename "$(dirname "$m")")-best.zip
  mv "$m" "$dst/$name/$x"
done

for m in "$1"/saved_models/*/model.zip ; do
  x=$(basename "$(dirname "$m")")-last.zip
  mv "$m" "$dst/$name/$x"
done

rm -r "$1"/saved_models
