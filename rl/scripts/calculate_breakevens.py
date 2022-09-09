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
import itertools
import numpy as np
from stable_baselines3 import A2C, PPO, DQN
from tqdm import tqdm

# TODO: Replicate last plots from https://github.com/pkel/cpr/blob/patrik/python/eval/gym-dense-reward-wrapper.ipynb
"""
Try K=5, 8, 15, 20
Evaluate on Patrik's wrapper
"""
protos = ["tailstorm"]
reward_schemes = ["constant", "discount"]
gammas = [0.9]
defenders = [50]
alphas = np.arange(0.1, 0.5, 0.01)
df = []
setups = itertools.product(protos, reward_schemes, alphas, gammas, defenders)
for setup in tqdm(setups, desc="Evaluating setups"):
    protocol = setup[0]
    reward_scheme = setup[1]
    alpha = setup[2]
    gamma = setup[3]
    defenders = setup[4]
    config = Config(
        STEPS_PER_ROLLOUT=200,
        ALPHA_SCHEDULE=[alpha],
        K=8,
        REWARD_SCHEME=reward_scheme,
        PROTOCOL=protocol,
        GAMMA=gamma,
        DEFENDERS=defenders,
    )
    env = env_fn(0, 1, config)
    env = AlphaScheduleWrapper(env, env_fn, config)
    # env = ReleaseOnDoneWrapper(env)
    # env = wrappers.DenseRewardPerBlockWrapper(env, config.STEPS_PER_ROLLOUT)
    env = SparseDaaRewardWrapper(env)
    for policy in env.policies():

        # p = PPO.load(f"rl/saved_models/model.zip")
        rs = []
        for _ in range(100):
            obs = env.reset()
            reward_attacker = 0
            done = False
            actions = []
            ep_r = 0
            while not done:
                # action, _state = p.predict(np.array(obs), deterministic=True)
                obs, r, done, info = env.step(env.policy(obs, policy))
                # obs, r, done, info = env.step(action)
                ep_r += r
                reward_attacker += info["step_reward_attacker"]
            rs.append(ep_r)
            df.append(
                dict(
                    protocol=protocol,
                    reward_scheme=reward_scheme,
                    alpha=alpha,
                    policy=policy,
                    reward=ep_r,
                    gamma=gamma,
                )
            )
df = pd.DataFrame(df)
print(df)
pd.to_pickle(df, "breakevens_gamma_0.9.pkl")
