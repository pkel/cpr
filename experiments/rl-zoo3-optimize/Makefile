repo := $(shell git rev-parse --show-toplevel)
base := $(shell realpath --relative-to . $(repo))

venv := $(base)/_venv

storage := sqlite:///cpr-rl-zoo3.db

.PHONY: $(venv)
$(venv):
	make -C $(base) _venv

# Show best trial
%.best: $(venv)
	$(venv)/bin/optuna best-trials -f yaml \
		--storage $(storage) \
		--study-name $*

# Trials

nakamoto-1e5: $(venv)
	$(venv)/bin/python -m rl_zoo3.train --algo ppo --yaml-file ppo.yaml \
		--gym-packages cpr_gym \
		--env 'cpr-nakamoto-v0' \
		--eval-episodes 256 \
		--n-eval-envs 4 \
		--optimize \
		--storage $(storage) \
		--study-name $@ \
		--n-timesteps 100000

tailstorm-1e7: $(venv)
	$(venv)/bin/python -m rl_zoo3.train --algo ppo --yaml-file ppo.yaml \
		--gym-packages cpr_gym \
		--env 'cpr-tailstorm-v0' \
		--n-eval-envs 4 \
		--eval-episodes 256 \
		--optimize \
		--storage $(storage) \
		--study-name $@ \
		--n-timesteps 10000000

tailstorm-1e7-dense: $(venv)
	$(venv)/bin/python -m rl_zoo3.train --algo ppo --yaml-file ppo.yaml \
		--gym-packages cpr_gym \
		--env 'cpr-tailstorm-v0' \
		--env-kwargs reward:\"dense_per_progress\" \
		--eval-episodes 256 \
		--n-eval-envs 4 \
		--optimize \
		--storage $(storage) \
		--study-name $@ \
		--n-timesteps 10000000

tailstorm-1e6-constant: $(venv)
	$(venv)/bin/python -m rl_zoo3.train --algo ppo --yaml-file ppo.yaml \
		--gym-packages cpr_gym \
		--env 'cpr-tailstorm-v0' \
		--env-kwargs "protocol_args:dict(reward='constant')" \
		--eval-episodes 256 \
		--n-eval-envs 4 \
		--optimize \
		--storage $(storage) \
		--study-name $@ \
		--n-timesteps 1000000
