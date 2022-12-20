from .config import config, Config
import gym
from cpr_gym import protocols
import numpy as np


def env_fn(alpha, target, config: Config):
    uo = config.UNIT_OBSERVATION
    if config.PROTOCOL == "nakamoto":
        proto = protocols.nakamoto(unit_observation=uo)
    elif config.PROTOCOL == "bk_ll":
        proto = protocols.bk_ll(k=config["K"], unit_observation=uo)
    elif config.PROTOCOL == "bk_ll":
        pass
    elif config.PROTOCOL == "tailstorm":
        proto = protocols.tailstorm(k=config.K, reward=config.REWARD_SCHEME, unit_observation=uo)
    elif config.PROTOCOL == "tailstormjune":
        proto = protocols.tailstormjune(k=config.K, reward=config.REWARD_SCHEME, unit_observation=uo)
    if config.USE_DAA:
        max_steps = config.STEPS_PER_ROLLOUT * 1000000
        max_time = config.STEPS_PER_ROLLOUT
    else:
        max_steps = config.STEPS_PER_ROLLOUT
        max_time = config.STEPS_PER_ROLLOUT * 1000000
    return gym.make(
        "cpr_gym:core-v0",
        proto=proto,
        alpha=alpha,
        max_steps=max_steps,
        max_time=max_time,
        # max_height=max_steps,
        gamma=config.GAMMA,
        defenders=config.DEFENDERS,
        activation_delay=target,
    )
