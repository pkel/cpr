#!/usr/bin/env bash

set -Eeuo pipefail

branch=origin/training

alphas=(20 25 30 35 40 45)
gammas=(05 50 95)
repeat=(1 2 3) # how often should each config be repeated?
protos=(
  nakamoto
  bk-8
  tailstorm-8-constant
)

hosts=(
  # 8/localhost
  4/athene
  4/iris
  # 4/nike
)
servers=$(printf ",%s" "${hosts[@]}")
servers=${servers:1}

# run once per host
setup () (
  set -Eeuo pipefail
  set -x
  {
    hostname

    git fetch
    git checkout "$branch"

    make python=python3.10 _venv build

  } 2>&1
)

# might run in parallel on individual host
ppo () (
  iteri=$1
  proto=$2
  alpha=$3
  gamma=$4
  echo "$proto" --alpha "$alpha" --gamma "$gamma" @ "$(hostname)"
  set -Eeuo pipefail
  set -x

  {
    # shellcheck disable=SC1091
    . _venv/bin/activate
    which python
    python --version

    cd experiments/train
    buf=$(mktemp ppo-XXXXXXXXX-out)
    python ppo.py "$proto" --alpha "$alpha" --gamma "$gamma" --batch --tag "$name" | tee "$buf"

    # locate and zip output directory
    out=$(grep -o "saved_models/ppo-$proto-alpha$alpha-gamma$gamma-[A-Za-z0-9-]*" "$buf")
    rm "$buf"
    zip ../../"ppo-$proto-alpha$alpha-gamma$gamma-$iteri.zip" -r "$out"

  } 2>&1
)

name=parallel
if [ $# -ge 1 ] ; then
  name=$1
fi

mkdir "$name.results"
cd "$name.results"

export -f ppo setup
export branch name

parallel -S "$servers" --nonall \
  --env setup --env branch --env name \
  --workdir cpr \
  --results setup \
  --joblog "job.log" \
  --eta \
  setup

parallel -S "$servers" \
  --controlmaster --sshdelay 0.1 \
  --env ppo --env name \
  --workdir cpr \
  --return "ppo-{proto}-alpha{alpha}-gamma{gamma}-{iteri}.zip" \
  --cleanup \
  --results "./ppo-{proto}-alpha{alpha}-gamma{gamma}-{iteri}" \
  --joblog "+job.log" \
  --eta \
  --header : \
  ppo \
  ::: iteri "${repeat[@]}" \
  ::: proto "${protos[@]}" \
  ::: alpha "${alphas[@]}" \
  ::: gamma "${gammas[@]}"
