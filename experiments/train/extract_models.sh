#!/usr/bin/env bash

set -Eeuo pipefail

dst=../../data/models

name=parallel
if [ $# -ge 1 ] ; then
  name=$1
fi

if [ ! -d "$name.results" ] ; then
  echo Result directory "$name.results" does not exist. Abort.
  exit 1
fi

if [ ! -d "$dst" ] ; then
  echo Target directory "$dst" does not exist. Abort.
  exit 2
fi

if [ -e "$dst/$name" ] ; then
  echo Target directory "$dst/$name" already exists. Abort.
  exit 3
fi

for z in "$name.results"/*.zip ; do
  unzip "$z" -d "$name.results/"
done

mkdir "$dst/$name"

for m in "$name.results"/saved_models/*/best_model.zip ; do
  x=$(basename "$(dirname "$m")")-best.zip
  mv "$m" "$dst/$name/$x"
done

for m in "$name.results"/saved_models/*/model.zip ; do
  x=$(basename "$(dirname "$m")")-last.zip
  mv "$m" "$dst/$name/$x"
done

rm -r "$name.results"/saved_models