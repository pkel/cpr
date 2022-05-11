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

# SM1_RESULTS = {
#     0: {
#         1 / 3.0: 1 / 3.0,
#         0.35: 0.36650,
#         0.375: 0.42118,
#         0.4: 0.48372,
#         0.425: 0.55801,
#         0.45: 0.65177,
#         0.475: 0.78254,
#     }
# }

config = dict(
    PROTOCOL="bk_ll",
    K=10,
    ALGO="PPO",
    TOTAL_TIMESTEPS=10e6,
    STEPS_PER_ROLLOUT=250,
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
    # ALPHA_SCHEDULE=[
    #     1 / 3.0,
    #     0.35,
    #     0.375,
    #     0.4,
    #     0.425,
    #     0.45,
    #     0.475,
    # ],
    USE_DAA=False,
    GAMMA=0,
    DEFENDERS=1,
    ACTIVATION_DELAY=1,
)
env = gym.make(
    "cpr-v0",
    spec=specs.bk_ll(
        k=config["K"],
        alpha=0.1,
        n_steps=250,
        gamma=config["GAMMA"],
        defenders=config["DEFENDERS"],
    ),
)
env = SparseRelativeRewardWrapper(env, relative=False)

p = PPO.load(f"saved_models/best_model.zip", env=env)
ALPHAS = list(np.arange(0.05, 0.5, 0.05))
for n_steps in [250]:
    alphas = []
    rewards = []
    sm1_rewards = []
    for alpha in ALPHAS:

        def env_fn(alpha, target, config):
            return gym.make(
                "cpr-v0",
                spec=specs.bk_ll(
                    k=config["K"],
                    alpha=0.1,
                    n_steps=250,
                    gamma=config["GAMMA"],
                    defenders=config["DEFENDERS"],
                ),
            )

        config["ALPHA_SCHEDULE"] = [alpha]
        env = env_fn(alpha, None, config)

        env = AlphaScheduleWrapper(env, env_fn, config)
        env = SparseRelativeRewardWrapper(env, relative=False)

        # env = WastedBlocksRewardWrapper(env)

        # model = A2C("MlpPolicy", env, verbose=1)
        # model.learn(total_timesteps=10000)
        sm1 = env.policies()["selfish"]
        # p = env.policies()["honest"]

        for _ in tqdm(range(1000)):
            obs = env.reset()
            done = False
            ep_r = 0
            while not done:
                sm1_action = sm1(np.array(obs))
                action, _state = p.predict(np.array(obs), deterministic=True)

                obs, r, done, info = env.step(action)

                ep_r += r
            rewards.append(ep_r)
            alphas.append(alpha)
        for _ in tqdm(range(1000)):
            obs = env.reset()
            done = False
            ep_r = 0
            while not done:
                sm1_action = sm1(np.array(obs))

                obs, r, done, info = env.step(sm1_action)

                ep_r += r
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
    ax.scatter(gb_mean["alpha"], gb_mean["alpha"], c="r")
    plt.xticks(ALPHAS)
    plt.title(
        f"Protocol: Bk_ll alpha vs reward for n_steps={n_steps} Gamma={config['GAMMA']}"
    )
    plt.legend(["RL", "Selfish", "Honest"])
plt.show()


"""
Average reward: 2721.0071208567556, Std: 150.17074259685518
Average relative reward: 0.5960710882917148, Std: 0.012208569807294437
Unique actions: {1: 533091, 3: 466909}
"""
