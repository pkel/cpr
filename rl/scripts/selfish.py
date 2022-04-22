import sys, os
from uuid import uuid4


sys.path.append(os.getcwd())

import numpy as np
import pandas as pd
import gym
from cpr_gym import specs
from stable_baselines3 import A2C
from tqdm import tqdm
import matplotlib.pyplot as plt
import seaborn as sns
from rl.wrappers.excess_reward_wrapper import (
    RelativeRewardWrapper,
    SparseRelativeRewardWrapper,
    WastedBlocksRewardWrapper,
)
from rl.wrappers.decreasing_alpha_wrapper import AlphaScheduleWrapper


for n_steps in [50, 100, 250, 500]:
    alphas = []
    rewards = []
    for alpha in np.arange(0.05, 0.5, 0.05):

        def alpha_schedule(step):
            return alpha

        def env_fn(alpha):
            return gym.make(
                "cpr-v0",
                spec=specs.nakamoto(alpha=alpha, n_steps=n_steps, gamma=0, defenders=1),
            )

        env = env_fn(alpha)
        env = AlphaScheduleWrapper(env, env_fn, alpha_schedule)
        env = WastedBlocksRewardWrapper(env)

        # model = A2C("MlpPolicy", env, verbose=1)
        # model.learn(total_timesteps=10000)
        p = env.policies()["sapirshtein-2016-sm1"]
        # p = env.policies()["honest"]

        for _ in tqdm(range(1000)):
            obs = env.reset()
            done = False
            ep_r = 0
            while not done:
                action = p(np.array(obs))
                obs, r, done, info = env.step(action)
                ep_r += r
            rewards.append(ep_r)
            alphas.append(alpha)

    df = pd.DataFrame({"alpha": alphas, "reward": rewards})
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
    ax.scatter(gb_mean["alpha"], gb_mean["alpha"], c="r")
    plt.xticks(np.arange(0.05, 0.5, 0.05))
    plt.title("alpha vs reward for n_steps={}".format(n_steps))
plt.show()


"""
Average reward: 2721.0071208567556, Std: 150.17074259685518
Average relative reward: 0.5960710882917148, Std: 0.012208569807294437
Unique actions: {1: 533091, 3: 466909}
"""
