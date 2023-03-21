#!/usr/bin/env bash

set -Eeuo pipefail

branch=origin/training

protos=(
  nakamoto
  bk-8
  tailstorm-8-constant
  tailstorm-8-discount
)
# protos=(dummy)
alphas=(50 45 40 35 30 25 20)
alphas=(50 45 40 35)
gammas=(05 50 95)
shapes=(raw exp cut)
shapes=(raw)
ent_coefs=(0.01 0.001 0.0001)
ent_coefs=(0.01)
learning_rates=(3e-3 1e-3 3e-4)
iteris=(1) # how often should each config be repeated?

hosts=(
  9/localhost
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

    nix-shell --command true || true # build shell
    eval "$(direnv export bash || true)" > /dev/null # load nix env

    make build test
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

    eval "$(direnv export bash || true)" > /dev/null # load nix env

    which python
    python --version

    mkdir -p "$dir"
    python ppo.py --batch --tag "$tag" "${@}" | tee "$dir/ppo-$jobnr.out"

    # locate and zip output directory
    # ( read all lines because sometimes, likely due to a bug,
    #   train.py creates or writes to a second directory with suffix _2 )
    grep -o "saved_models/ppo-[A-Za-z0-9_.-]*" "$dir/ppo-$jobnr.out" | \
      while read -r out ; do
        zip "$dir/ppo-$jobnr.zip" -r "$out"
      done

  } 2>&1
)

tag=parallel
if [ $# -ge 1 ] ; then
  tag=$1
fi

remotedir=.cpr-training

dir=_parallel/$tag
mkdir -p _parallel
mkdir "$dir"

export -f ppo setup
export branch tag dir

parallel -S "$servers" --nonall \
  --env setup --env branch --env tag \
  --workdir $remotedir \
  --results setup \
  --joblog "$dir/setup.job.log" \
  --eta \
  setup

parallel -S "$servers" \
  --controlmaster --sshdelay 0.1 \
  --env ppo --env tag --env dir --env WANDB_MODE \
  --workdir $remotedir/experiments/train \
  --return "$dir/ppo-{#}.zip" \
  --results "$dir/ppo-{#}" \
  --joblog "$dir/ppo.job.log" \
  --retries 1 \
  --eta \
  --header : \
  ppo "{#}" "{proto}" --alpha "{alpha}" --gamma "{gamma}" --shape "{shape}" --ent_coef "{ent_coef}" --learning_rate "{learning_rate}" \
  ::: iteri "${iteris[@]}" \
  ::: proto "${protos[@]}" \
  ::: alpha "${alphas[@]}" \
  ::: gamma "${gammas[@]}" \
  ::: shape "${shapes[@]}" \
  ::: ent_coef "${ent_coefs[@]}" \
  ::: learning_rate "${learning_rates[@]}"
