import sys, os, psutil, time
from uuid import uuid4

sys.path.append(os.getcwd())

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

import numpy as np
from stable_baselines3 import A2C, PPO, DQN


# TODO: Replicate last plots from https://github.com/pkel/cpr/blob/patrik/python/eval/gym-dense-reward-wrapper.ipynb
"""
Try K=5, 8, 15, 20
Evaluate on Patrik's wrapper
"""

config = Config(
    STEPS_PER_ROLLOUT=200,
    ALPHA_SCHEDULE=[0.35],
    K=8,
    REWARD_SCHEME="constant",
    USE_DAA=False,
)
env = env_fn(0, 1, config)
env = AlphaScheduleWrapper(env, env_fn, config)
# env = ReleaseOnDoneWrapper(env)
env = wrappers.DenseRewardPerBlockWrapper(env, config.STEPS_PER_ROLLOUT)
# env = SparseDaaRewardWrapper(env)
p = PPO.load(f"rl/saved_models/model.zip")
rs = []
while True:
    obs = env.reset()
    reward_attacker = 0
    done = False
    actions = []
    ep_r = 0
    while not done:
        action, _state = p.predict(np.array(obs), deterministic=True)
        actions.append(action)
        # obs, r, done, info = env.step(env.policy(obs, "override-catchup"))
        obs, r, done, info = env.step(action)
        ep_r += r
        reward_attacker += info["step_reward_attacker"]
    print(reward_attacker)
    print(ep_r)
    rs.append(ep_r)
    print("---")
