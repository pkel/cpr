#!/usr/bin/env bash

set -Eeuo pipefail

branch=origin/training

protos=(
  nakamoto
  # bk-8
  # tailstorm-8-constant
)
alphas=(20 25 30 35 40 45)
alphas=(40)
gammas=(05 50 95)
gammas=(05)
shapes=(raw cut exp)
iteris=(1 2) # how often should each config be repeated?

hosts=(
  # 6/localhost
  # 4/athene
  # 4/iris
  # 4/nike
  2/athene
  2/iris
  2/nike
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
  jobnr=$1
  shift

  set -Eeuo pipefail

  {
    # shellcheck disable=SC1091
    . _venv/bin/activate
    which python
    python --version

    set -x
    cd experiments/train
    buf=$(mktemp ppo-XXXXXXXXX-out)
    echo python ppo.py --batch --tag "$name" "${@}" | tee "$buf"

    locate and zip output directory
    out=$(grep -o "saved_models/ppo-[A-Za-z0-9-]*" "$buf")
    rm "$buf"
    zip ../../"ppo-$name-$jobnr.zip" -r "$out"

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
  --joblog "setup.job.log" \
  --eta \
  setup

parallel -S "$servers" \
  --controlmaster --sshdelay 0.1 \
  --env ppo --env name \
  --workdir cpr \
  --return "ppo-$name-{#}.zip" \
  --cleanup \
  --results "./ppo-$name-{#}" \
  --joblog "ppo.job.log" \
  --retries 2 \
  --eta \
  --header : \
  ppo "{#}" "{proto}" --alpha "{alpha}" --gamma "{gamma}" --shape "{shape}" \
  ::: proto "${protos[@]}" \
  ::: alpha "${alphas[@]}" \
  ::: gamma "${gammas[@]}" \
  ::: shape "${shapes[@]}" \
  ::: iteri "${iteris[@]}"
