```shell
# activate Python venv
. ../../_venv/bin/activate

# build and install the package / gym
maturin develop --release

# start a simple training run
python -m rl_zoo3.train --env 'FC16SSZwPT-v0' --env-kwargs alpha:0.33 gamma:1.0 --gym-packages cpr_gym_rs --algo ppo --conf hyperparams/ppo.yml --eval-freq 0 -P
```
