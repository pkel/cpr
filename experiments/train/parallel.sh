#!/usr/bin/env bash

set -Eeuo pipefail

branch=origin/training

protos=(
  nakamoto
  bk-8
  tailstorm-8-constant
  tailstorm-8-discount
)
alphas=(50 45 40 35 30 25 20)
gammas=(05 50 95)
shapes=(raw exp cut)
shapes=(raw)
ent_coefs=(0.01 0.001 0.0001)
iteris=(1) # how often should each config be repeated?

hosts=(
  16/localhost
  # 4/athene
  # 4/iris
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

    make _venv build

  } 2>&1
)

# might run in parallel on individual host
ppo () (
  jobnr=$1
  shift

  set -Eeuo pipefail

  {
    root=$(git rev-parse --show-toplevel)

    # shellcheck disable=SC1091
    . "$root/_venv/bin/activate"

    set -x

    which python
    python --version

    mkdir -p "$dir"
    python ppo.py --batch --tag "$name" "${@}" | tee "$dir/ppo-$jobnr.out"

    # locate and zip output directory
    # ( read all lines because sometimes, likely due to a bug,
    #   train.py creates or writes to a second directory with suffix _2 )
    grep -o "saved_models/ppo-[A-Za-z0-9_.-]*" "$dir/ppo-$jobnr.out" | \
      while read -r out ; do
        zip "$dir/ppo-$jobnr.zip" -r "$out"
      done

  } 2>&1
)

name=parallel
if [ $# -ge 1 ] ; then
  name=$1
fi

remotedir=.cpr-training

dir=_parallel/$name
mkdir -p _parallel
mkdir "$dir"

export -f ppo setup
export branch name dir

parallel -S "$servers" --nonall \
  --env setup --env branch --env name \
  --workdir $remotedir \
  --results setup \
  --joblog "$dir/setup.job.log" \
  --eta \
  setup

parallel -S "$servers" \
  --controlmaster --sshdelay 0.1 \
  --env ppo --env name --env dir \
  --workdir $remotedir/experiments/train \
  --return "$dir/ppo-{#}.zip" \
  --results "$dir/ppo-{#}" \
  --joblog "$dir/ppo.job.log" \
  --retries 2 \
  --eta \
  --header : \
  ppo "{#}" "{proto}" --alpha "{alpha}" --gamma "{gamma}" --shape "{shape}" --ent_coef "{ent_coef}" \
  ::: iteri "${iteris[@]}" \
  ::: proto "${protos[@]}" \
  ::: alpha "${alphas[@]}" \
  ::: gamma "${gammas[@]}" \
  ::: shape "${shapes[@]}" \
  ::: ent_coef "${ent_coefs[@]}"
