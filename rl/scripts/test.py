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
from stable_baselines3 import A2C, PPO, DQN

SM1_RESULTS = {
    0: {
        1 / 3.0: 1 / 3.0,
        0.35: 0.36650,
        0.375: 0.42118,
        0.4: 0.48372,
        0.425: 0.55801,
        0.45: 0.65177,
        0.475: 0.78254,
    }
}

GAMMA = 0
DEFENDERS = 1
env = gym.make(
    "cpr-v0",
    spec=specs.nakamoto(alpha=0.1, n_steps=250, gamma=GAMMA, defenders=DEFENDERS),
)
env = SparseRelativeRewardWrapper(env, relative=False)

p = PPO.load(f"saved_models/best_model.zip", env=env)
for n_steps in [250]:
    alphas = []
    rewards = []
    for alpha in SM1_RESULTS[GAMMA].keys():

        def env_fn(alpha, target):
            return gym.make(
                "cpr-v0",
                spec=specs.nakamoto(
                    alpha=alpha, n_steps=n_steps, gamma=GAMMA, defenders=DEFENDERS
                ),
            )

        env = env_fn(alpha, None)
        env = AlphaScheduleWrapper(env, env_fn, [alpha])
        env = SparseRelativeRewardWrapper(env, relative=False)
        # env = WastedBlocksRewardWrapper(env)

        # model = A2C("MlpPolicy", env, verbose=1)
        # model.learn(total_timesteps=10000)
        sm1 = env.policies()["sapirshtein-2016-sm1"]
        # p = env.policies()["honest"]

        for _ in tqdm(range(1000)):
            obs = env.reset()
            done = False
            ep_r = 0
            while not done:
                sm1_action = sm1(np.array(obs))
                action, _state = p.predict(np.array(obs))

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
    ax.scatter(
        list(SM1_RESULTS[GAMMA].keys()), list(SM1_RESULTS[GAMMA].values()), c="green"
    )
    ax.scatter(gb_mean["alpha"], gb_mean["alpha"], c="r")
    plt.xticks(list(SM1_RESULTS[GAMMA].keys()))
    plt.title("alpha vs reward for n_steps={}".format(n_steps))
    plt.legend(["RL", "SM1", "Honest"])
plt.show()


"""
Average reward: 2721.0071208567556, Std: 150.17074259685518
Average relative reward: 0.5960710882917148, Std: 0.012208569807294437
Unique actions: {1: 533091, 3: 466909}
"""
