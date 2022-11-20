repo := $(shell git rev-parse --show-toplevel)
base := $(shell realpath --relative-to . $(repo))

venv := $(base)/_venv

storage := sqlite:///cpr-ppo.db

.PHONY: $(venv)
$(venv):
	make -C $(base) _venv

# Show best trial
%.best: $(venv)
	$(venv)/bin/optuna best-trials -f yaml \
		--storage $(storage) \
		--study-name $*

# Trials

nakamoto: $(venv)
	$(venv)/bin/python -m rl_zoo3.train --algo ppo --yaml-file ppo.yaml \
		--gym-packages cpr_gym \
		--env 'cpr-nakamoto-v0' \
		--optimize \
		--storage $(storage) \
		--study-name $@ \
		--eval-episodes 64 \
		--n-timesteps 100000

tailstorm: $(venv)
	$(venv)/bin/python -m rl_zoo3.train --algo ppo --yaml-file ppo.yaml \
		--gym-packages cpr_gym \
		--env 'cpr-tailstorm-v0' \
		--optimize \
		--storage $(storage) \
		--study-name $@ \
		--eval-episodes 64 \
		--n-timesteps 1000000

tailstorm-constant: $(venv)
	$(venv)/bin/python -m rl_zoo3.train --algo ppo --yaml-file ppo.yaml \
		--gym-packages cpr_gym \
		--env 'cpr-tailstorm-v0' \
		--env-kwargs "protocol_args:dict(reward='constant')" \
		--optimize \
		--storage $(storage) \
		--study-name $@ \
		--eval-episodes 64 \
		--n-timesteps 1000000
