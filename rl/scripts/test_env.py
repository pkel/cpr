import sys, os, psutil, time
from uuid import uuid4

sys.path.append(os.getcwd())

import gym
from cpr_gym import protocols
from rl.wrappers.excess_reward_wrapper import (
    RelativeRewardWrapper,
    SparseDaaRewardWrapper,
    SparseRelativeRewardWrapper,
    WastedBlocksRewardWrapper,
    AbsoluteRewardWrapper,
)
from rl.wrappers import ReleaseOnDoneWrapper

from rl.wrappers.decreasing_alpha_wrapper import AlphaScheduleWrapper


def env_fn(alpha, target, config):
    if config["PROTOCOL"] == "nakamoto":
        proto = protocols.nakamoto()
    elif config["PROTOCOL"] == "bk_ll":
        proto = protocols.bk_ll(k=config["K"])
    elif config["PROTOCOL"] == "bk_ll":
        pass
    elif config["PROTOCOL"] == "tailstorm":
        proto = protocols.tailstorm(k=config["K"], reward="constant")

    if config["USE_DAA"]:
        max_steps = config["STEPS_PER_ROLLOUT"] * 1000
        max_time = config["STEPS_PER_ROLLOUT"]
    else:
        max_steps = config["STEPS_PER_ROLLOUT"]
        max_time = config["STEPS_PER_ROLLOUT"] * 1000
    return gym.make(
        "cpr_gym:core-v0",
        proto=proto,
        alpha=alpha,
        max_steps=max_steps,
        max_time=max_time,
        gamma=config["GAMMA"],
        defenders=config["DEFENDERS"],
        activation_delay=target,
    )


config = dict(
    PROTOCOL="nakamoto",
    K=10,
    ALGO="PPO",
    TOTAL_TIMESTEPS=10e8,
    STEPS_PER_ROLLOUT=5000,
    STARTING_LR=10e-5,
    ENDING_LR=10e-6,
    BATCH_SIZE=2048,
    ALPHA_SCHEDULE_CUTOFF=0,
    LAYER_SIZE=100,
    N_LAYERS=2,
    N_STEPS_MULTIPLE=10,
    HONEST_STEPS_FRACTION=0.1,
    STARTING_EPS=0.99,
    ENDING_EPS=0.01,
    ALPHA_SCHEDULE=[0.25],
    USE_DAA=True,
    DAA_METHOD="sparse",
    GAMMA=0,
    DEFENDERS=1,
    ACTIVATION_DELAY=1,
    N_ENVS=16,
)
env = env_fn(0, 1, config)
env = AlphaScheduleWrapper(env, env_fn, config)

env = SparseDaaRewardWrapper(env)
while True:
    obs = env.reset()
    reward_attacker = 0
    done = False
    while not done:
        obs, r, done, info = env.step(env.policy(obs, "honest"))
        reward_attacker += info["reward_attacker"]
    print(reward_attacker)
    print(r)
    print("---")
