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
    STEPS_PER_ROLLOUT=2016,
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
        "cpr-v0",
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
    env = AbsoluteRewardWrapper(env)
else:
    env = SparseRelativeRewardWrapper(env, relative=False)

p = PPO.load(f"saved_models/best_model_nakamoto_gamma_0.zip", env=env)
ALPHAS = list(np.arange(0.05, 0.5, 0.05))
alphas = []
rewards = []
sm1_rewards = []
for alpha in ALPHAS:

    config["ALPHA_SCHEDULE"] = [alpha]
    env = env_fn(alpha, 1, config)

    env = AlphaScheduleWrapper(env, env_fn, config)
    if config["USE_DAA"]:
        env = AbsoluteRewardWrapper(env)
    else:
        env = SparseRelativeRewardWrapper(env, relative=False)

    sm1 = env.policies["sapirshtein-2016-sm1"]

    for i in tqdm(range(1000)):
        obs = env.reset()
        done = False
        ep_r = 0
        while not done:
            sm1_action = sm1(np.array(obs))
            action, _state = p.predict(np.array(obs), deterministic=True)

            obs, r, done, info = env.step(action)

            ep_r += r
        if not config["USE_DAA"] or (config["USE_DAA"] and i > 100):
            rewards.append(ep_r)
            alphas.append(alpha)
    obs = env.reset(reset_difficulties=True)
    for i in tqdm(range(1000)):
        obs = env.reset()
        done = False
        ep_r = 0
        while not done:
            sm1_action = sm1(np.array(obs))

            obs, r, done, info = env.step(sm1_action)

            ep_r += r
        if not config["USE_DAA"] or (config["USE_DAA"] and i > 100):
            sm1_rewards.append(ep_r)

df = pd.DataFrame({"alpha": alphas, "reward": rewards, "sm1_reward": sm1_rewards})
gb_mean = df.groupby("alpha").mean().reset_index()
gb_std = df.groupby("alpha").std().reset_index()
fig, ax = plt.subplots()
ax.scatter(gb_mean["alpha"], gb_mean["reward"], c="b")
ax.errorbar(
    gb_mean["alpha"],
    gb_mean["reward"],
    yerr=gb_std["reward"],
    fmt="none",
    ecolor="b",
)
ax.scatter(gb_mean["alpha"], gb_mean["sm1_reward"], c="green")
if not config["USE_DAA"]:
    ax.scatter(gb_mean["alpha"], gb_mean["alpha"], c="r")
else:
    ax.scatter(gb_mean["alpha"], gb_mean["alpha"] * config["STEPS_PER_ROLLOUT"], c="r")
plt.xticks(ALPHAS)
plt.title(
    f"Protocol: {config['PROTOCOL']} alpha vs reward for n_steps={config['STEPS_PER_ROLLOUT']} Gamma={config['GAMMA']}"
)
plt.legend(["RL", "Selfish", "Honest"])
plt.show()


"""
Average reward: 2721.0071208567556, Std: 150.17074259685518
Average relative reward: 0.5960710882917148, Std: 0.012208569807294437
Unique actions: {1: 533091, 3: 466909}
"""
