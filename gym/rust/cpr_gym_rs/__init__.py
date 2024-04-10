from . import _rust
from gymnasium.envs.registration import register

default_assumptions = dict(
    alpha=0.42,
    gamma=0.84,
    horizon=100,
)

register(
    id="FC16SSZwPT-v0",
    entry_point="cpr_gym_rs.envs:FC16SSZwPT",
    nondeterministic=True,
    order_enforce=False,
    max_episode_steps=1000,  # forced termination, not Markovian
    kwargs=default_assumptions,
)

register(
    id="Nakamoto-v0",
    entry_point="cpr_gym_rs.envs:Generic",
    nondeterministic=True,
    order_enforce=False,
    max_episode_steps=1000,  # forced termination, not Markovian
    kwargs=dict(protocol="nakamoto") | default_assumptions,
)
