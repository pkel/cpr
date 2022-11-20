repo := $(shell git rev-parse --show-toplevel)
base := $(shell realpath --relative-to . $(repo))

venv := $(base)/_venv

storage := sqlite:///cpr-ppo.db

.PHONY: $(venv)
$(venv):
	make -C $(base) _venv

# Trials

nakamoto: $(venv)
	$(venv)/bin/python -m rl_zoo3.train --algo ppo --yaml-file ppo.yaml \
		--gym-packages cpr_gym \
		--env 'cpr-nakamoto-v0' \
		--optimize \
		--storage $(storage) \
		--study-name $@ \
		--eval-episodes 32 \
		--n-timesteps 100000 \
		|| true
	$(venv)/bin/optuna best-trials -f yaml \
		--storage $(storage) \
		--study-name $@

tailstorm: $(venv)
	$(venv)/bin/python -m rl_zoo3.train --algo ppo --yaml-file ppo.yaml \
		--gym-packages cpr_gym \
		--env 'cpr-tailstorm-v0' \
		--optimize \
		--storage $(storage) \
		--study-name $@ \
		--eval-episodes 32 \
		--n-timesteps 300000 \
		|| true
	$(venv)/bin/optuna best-trials -f yaml \
		--storage $(storage) \
		--study-name $@

tailstorm-constant: $(venv)
	$(venv)/bin/python -m rl_zoo3.train --algo ppo --yaml-file ppo.yaml \
		--gym-packages cpr_gym \
		--env 'cpr-tailstorm-v0' \
		--env-kwargs "protocol_args:dict(reward='constant')" \
		--optimize \
		--storage $(storage) \
		--study-name $@ \
		--eval-episodes 32 \
		--n-timesteps 300000 \
		|| true
	$(venv)/bin/optuna best-trials -f yaml \
		--storage $(storage) \
		--study-name $@
