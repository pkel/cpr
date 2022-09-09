import itertools
import sys, os, psutil, time
from uuid import uuid4

sys.path.append(os.getcwd())
import pandas as pd
import gym
from cpr_gym import protocols, wrappers
from rl.wrappers.excess_reward_wrapper import (
    RelativeRewardWrapper,
    SparseDaaRewardWrapper,
    SparseRelativeRewardWrapper,
    WastedBlocksRewardWrapper,
    AbsoluteRewardWrapper,
)
from rl.wrappers import ReleaseOnDoneWrapper
from rl.utils import env_fn, Config
from rl.wrappers.decreasing_alpha_wrapper import AlphaScheduleWrapper
from tqdm import tqdm
import numpy as np
from stable_baselines3 import A2C, PPO, DQN


# TODO: Replicate last plots from https://github.com/pkel/cpr/blob/patrik/python/eval/gym-dense-reward-wrapper.ipynb
"""
Try K=5, 8, 15, 20
Evaluate on Patrik's wrapper
"""
reward_schemes = ["constant"]
protos = ["nakamoto"]
alphas = np.arange(0.1, 0.5, 0.01)
# alphas = [0.26]
gammas = [0, 0.5, 0.9]
defenders = 50
model_names = ["nakamoto"]
# model_names = ["2x496teq"]
df = []
loaded_models = dict()
setups = list(itertools.product(reward_schemes, alphas, model_names, protos, gammas))
for setup in tqdm(setups):
    reward_scheme = setup[0]
    alpha = setup[1]
    model_name = setup[2]
    protocol = setup[3]
    gamma = setup[4]
    if model_name not in loaded_models:
        p = PPO.load(f"rl/saved_models/model_{model_name}.zip")
        loaded_models[model_name] = p
    else:
        p = loaded_models[model_name]

    config = Config(
        PROTOCOL=protocol,
        STEPS_PER_ROLLOUT=200,
        ALPHA_SCHEDULE=[alpha],
        K=8,
        REWARD_SCHEME=reward_scheme,
        GAMMA=gamma,
        DEFENDERS=50 if gamma == 0.9 else 2,
    )
    env = env_fn(0, 1, config)
    env = AlphaScheduleWrapper(env, env_fn, config)
    # env = ReleaseOnDoneWrapper(env)
    # env = wrappers.DenseRewardPerBlockWrapper(env, config.STEPS_PER_ROLLOUT)
    env = SparseDaaRewardWrapper(env)
    for _ in range(1000):
        obs = env.reset()
        reward_attacker = 0
        done = False
        actions = []
        ep_r = 0
        while not done:
            action, _state = p.predict(np.array(obs), deterministic=True)
            actions.append(action)
            # obs, r, done, info = env.step(env.policy(obs, "honest"))
            obs, r, done, info = env.step(action)
            ep_r += r
            reward_attacker += info["step_reward_attacker"]
        df.append(
            dict(
                protocol=protocol,
                reward_scheme=reward_scheme,
                alpha=alpha,
                policy=model_name,
                reward=ep_r,
                gamma=gamma,
            )
        )
df = pd.DataFrame(df)
pd.to_pickle(df, f"rl/saved_models/model_evals_all_gammas_nakamoto.pkl")
