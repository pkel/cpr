#!/usr/bin/env bash

set -Eeuo pipefail

branch=origin/dag-voting

protos=(
  # nakamoto
  spar-8
  # stree-8-constant
  stree-8-discount
  # sdag-8-constant
  sdag-8-discount
  # spar-4
  # stree-4-constant
  # stree-4-discount
  # sdag-4-constant
  # sdag-4-discount
  # tailstorm-4-discount
)
# protos=(dummy)
alphas=(50 45 40 35 30 25 20)
alphas=(45 40 35 30 25) # target for dag-voting
gammas=(05 50 95) # target for dag-voting
gammas=(05)
shapes=(raw)
ent_coefs=(0.001 0.005 0.01) # target for dag-voting
ent_coefs=(0.005)
learning_rates=(1e-3 3e-4) # target for dag-voting
iteris=(1) # how often should each config be repeated? Once for dag-voting

# dag-voting: this makes 18 runs per combination of gamma and alpha or about
# 28.5h of training on teide.
# (Without Nakamoto, which I can reuse from Tailstorm.)
# Complete: point alpha=40 and gamma=95.
# Running: other alphas, gamma=95, ent_coef 0.005. ETA: Sat Nov 11, 05:00 (rest till 09:00)
# Planned: gamma=05, all alphas, ent_coef 0.005. 30 runs; 47.5h. ETA: Mon Nov 13, 08:30
# Planned: gamma=05, all alpha, other ent_coefs. 60 runs; 95h. ETA: Fri Nov 17, 7:30
# Planned: gamma=50. 90 runs; 142.4h. ETA: Wed Nov 22, 22:30

hosts=(
  6/localhost # on teide
  # 16/localhost # on lennie, faster than 12
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

    eval "$(direnv export bash || true)" > /dev/null # load nix env

    # shellcheck disable=SC1091
    . "$root/_venv/bin/activate"

    set -x

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
  ::: ent_coef "${ent_coefs[@]}" \
  ::: learning_rate "${learning_rates[@]}" \
  ::: shape "${shapes[@]}" \
  ::: gamma "${gammas[@]}" \
  ::: alpha "${alphas[@]}" \
  ::: proto "${protos[@]}"
# topmost variable is outermost loop; check no \ on the last line!
