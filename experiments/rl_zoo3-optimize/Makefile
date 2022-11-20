python = ../../_venv/bin/python

# Nakamoto
cpr-ppo:
	$(python) -m rl_zoo3.train --algo ppo --gym-packages cpr_gym --env 'cpr-v0' \
		--yaml-file ppo.yaml --env-kwargs alpha:0.45 max_steps:100 \
		protocol:\'nakamoto\' gamma:0 reward:\'sparse_relative\' \
		--eval-episodes 16 --n-eval-envs 1 \
		--optimize --n-timesteps 100000 \

# Tailstorm 8 constant
tailstorm-8-constant-0:
	$(python) -m rl_zoo3.train --algo ppo --gym-packages cpr_gym --env 'cpr-v0' \
		--yaml-file ppo.yaml --env-kwargs alpha:0.45 max_steps:100 \
		protocol:\'tailstorm\' \
		protocol_args:"dict(k=8, reward='constant', subblock_selection='heuristic')" \
		gamma:0 reward:\'sparse_relative\' \
		--eval-episodes 16 --n-eval-envs 1 \
		--optimize --n-timesteps 300000 \
		--study-name $@ --storage sqlite:///cpr-ppo.db	--study-name $@ --storage sqlite:///cpr-ppo.db

# Tailstorm 8 discount
tailstorm-8-discount-0:
	$(python) -m rl_zoo3.train --algo ppo --gym-packages cpr_gym --env 'cpr-v0' \
		--yaml-file ppo.yaml --env-kwargs alpha:0.45 max_steps:100 \
		protocol:\'tailstorm\' \
		protocol_args:"dict(k=8, reward='discount', subblock_selection='heuristic')" \
		gamma:0 reward:\'sparse_relative\' \
		--eval-episodes 16 --n-eval-envs 1 \
		--optimize --n-timesteps 300000 \
		--study-name $@ --storage sqlite:///cpr-ppo.db	--study-name $@ --storage sqlite:///cpr-ppo.db
