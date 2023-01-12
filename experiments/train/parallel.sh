#!/usr/bin/env bash

set -Eeuo pipefail

branch=origin/training

alphas=(33 40 45)
gammas=(05 50 95)
protos=(
  nakamoto
  # bk-8
  # tailstorm-8-constant
)

hosts=(
  1/localhost
  1/athene
  1/iris
  1/nike
)
servers=$(printf ",%s" "${hosts[@]}")
servers=${servers:1}

ppo () (
  proto=$1
  alpha=$2
  gamma=$3
  echo "$proto" --alpha "$alpha" --gamma "$gamma" @ "$(hostname)"
  set -Eeuo pipefail
  set -x

  {
    git fetch
    git checkout "$branch"

    make python=python3.10 _venv build

    # TODO it might be useful to run the above commands once per batch in
    # advance. Then version is used of batch start time, not job start time.

    # shellcheck disable=SC1091
    . _venv/bin/activate
    which python
    python --version

    cd experiments/train
    python ppo.py "$proto" --alpha "$alpha" --gamma "$gamma" --batch --tag "$name"

    cd saved_models
    # shellcheck disable=SC2012
    ret=$(ls -td ./*/ | head -n 1)
    zip ../../../"ppo-$proto-alpha$alpha-gamma$gamma.zip" -r "$ret"
  } 2>&1
)

name=parallel
if [ $# -ge 1 ] ; then
  name=$1
fi

mkdir "$name.results"
cd "$name.results"

export -f ppo
export branch name

parallel -S "$servers" \
  --env ppo --env branch --env name \
  --workdir cpr \
  --return "ppo-{proto}-alpha{alpha}-gamma{gamma}.zip" \
  --cleanup \
  --results "./ppo-{proto}-alpha{alpha}-gamma{gamma}" \
  --joblog "job.log" \
  --progress \
  --header : \
  ppo \
  ::: proto "${protos[@]}" \
  ::: alpha "${alphas[@]}" \
  ::: gamma "${gammas[@]}"
