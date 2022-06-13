import sys, os
from uuid import uuid4


sys.path.append(os.getcwd())

import numpy as np
import pandas as pd
import gym
from cpr_gym import protocols
from stable_baselines3 import A2C
from tqdm import tqdm
import matplotlib.pyplot as plt
import seaborn as sns
from rl.wrappers.excess_reward_wrapper import (
    RelativeRewardWrapper,
    SparseDaaRewardWrapper,
    SparseRelativeRewardWrapper,
    WastedBlocksRewardWrapper,
    AbsoluteRewardWrapper,
)
from rl.wrappers.decreasing_alpha_wrapper import AlphaScheduleWrapper
from stable_baselines3 import A2C, PPO, DQN

config = dict(
    PROTOCOL="nakamoto",
    K=10,
    ALGO="PPO",
    TOTAL_TIMESTEPS=10e6,
    STEPS_PER_ROLLOUT=2000,
    STARTING_LR=10e-5,
    ENDING_LR=10e-7,
    BATCH_SIZE=2048,
    ALPHA_SCHEDULE_CUTOFF=0,
    LAYER_SIZE=100,
    N_LAYERS=2,
    N_STEPS_MULTIPLE=10,
    HONEST_STEPS_FRACTION=0.1,
    STARTING_EPS=0.99,
    ENDING_EPS=0.01,
    USE_DAA=True,
    DAA_METHOD="sparse",
    GAMMA=0,
    DEFENDERS=1,
    ACTIVATION_DELAY=1,
)


def env_fn(alpha, target, config):
    if config["PROTOCOL"] == "nakamoto":
        proto = protocols.nakamoto()
    elif config["PROTOCOL"] == "bk_ll":
        proto = protocols.bk_ll(k=config["K"])

    if config["USE_DAA"]:
        max_steps = config["STEPS_PER_ROLLOUT"] * 10
        max_time = config["STEPS_PER_ROLLOUT"]
    else:
        max_steps = config["STEPS_PER_ROLLOUT"]
        max_time = config["STEPS_PER_ROLLOUT"] * 10
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


env = env_fn(0.1, 1, config)
if config["USE_DAA"]:
    if config["DAA_METHOD"] == "sparse":
        env = SparseDaaRewardWrapper(env)
    else:
        env = AbsoluteRewardWrapper(env)
else:
    env = SparseRelativeRewardWrapper(env, relative=False)


model = PPO(
    "MlpPolicy",
    env,
)
# p = PPO.load(f"rl/saved_models/best_model", env=env)

ALPHAS = list(np.arange(0.5, 0.55, 0.05))
difficulties = dict((alpha, []) for alpha in ALPHAS)
for n_steps in [2016]:
    alphas = []
    rewards = []
    sm1_rewards = []
    for alpha in ALPHAS:

        config["ALPHA_SCHEDULE"] = [0]
        env = env_fn(0, 1, config)

        env = AlphaScheduleWrapper(env, env_fn, config)
        if config["USE_DAA"]:
            if config["DAA_METHOD"] == "sparse":
                env = SparseDaaRewardWrapper(env)
            else:
                env = AbsoluteRewardWrapper(env)
        else:
            env = SparseRelativeRewardWrapper(env, relative=False)

        sm1 = env.policies["sapirshtein-2016-sm1"]
        for i in tqdm(range(100)):
            obs = env.reset()
            done = False
            ep_r = 0
            while not done:
                action = sm1(np.array(obs)[:4])
                # action, _state = p.predict(np.array(obs), deterministic=True)

                obs, r, done, info = env.step(3)
                ep_r += r
            print(ep_r)
            # difficulties[alpha].append(info["difficulties"][alpha])

    fig, ax = plt.subplots()
    for key, value in difficulties.items():
        ax.plot(value, label=key)
    plt.legend(list(difficulties.keys()))
    plt.title(f"Difficulties for sm1")

plt.show()
