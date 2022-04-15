import sys, os

sys.path.append(os.getcwd())
import numpy as np

import gym
from cpr_gym import specs
from stable_baselines3 import A2C, PPO, DQN
from stable_baselines3.ppo.policies import MlpPolicy
from tqdm import tqdm
import torch
from stable_baselines3.common.env_util import make_vec_env
from rl.wrappers.exploration_reward_wrapper import ExplorationRewardWrapper
from rl.wrappers.excess_reward_wrapper import (
    RelativeRewardWrapper,
    SparseRelativeRewardWrapper,
)
from rl.wrappers.decreasing_alpha_wrapper import AlphaScheduleWrapper

ALPHA = 0.35
ALGO = "DQN"
TOTAL_TIMESTEPS = 10e6
STEPS_PER_ROLLOUT = 100


def lr_schedule(remaining):
    return 10e-4 * remaining + 10e-7 * (1 - remaining)


def clip_schedule(remaining):
    return 0.1


def env_fn(alpha):
    return gym.make(
        "cpr-v0", spec=specs.nakamoto(alpha=alpha, n_steps=STEPS_PER_ROLLOUT)
    )


def alpha_schedule(step):
    progress = step / (TOTAL_TIMESTEPS / STEPS_PER_ROLLOUT)
    if progress > 0.5:
        alpha = np.random.normal(0.3, 0.1)
        alpha = min(alpha, 0.49)
        alpha = max(alpha, 0.25)
    else:
        alpha = 0.25 * progress + 0.5 * (1 - progress)
    return alpha


env = gym.make("cpr-v0", spec=specs.nakamoto(alpha=ALPHA, n_steps=STEPS_PER_ROLLOUT))
# env = AlphaScheduleWrapper(env, env_fn, alpha_schedule)
env = SparseRelativeRewardWrapper(env, alpha=ALPHA)
# env = ExplorationRewardWrapper(env, alpha=ALPHA, max_steps=1000)
# env = make_vec_env(env, n_envs=4)

# policy_kwargs = dict(
#     activation_fn=torch.nn.ReLU,
#     net_arch=[dict(pi=[100, 100], vf=[100, 100])],
# )
if ALGO == "PPO":
    model = PPO(
        "MlpPolicy",
        env,
        verbose=1,
        # batch_size=64000,
        # n_steps=64000 * 2,
        # clip_range=0.1,
        # ent_coef=0.01,
        # learning_rate=lr_schedule,
        # clip_range=clip_schedule,
        # policy_kwargs=policy_kwargs,
    )
elif ALGO == "DQN":
    model = DQN(
        "MlpPolicy",
        env,
        verbose=1,
        batch_size=64000,
        # clip_range=0.1,
        # ent_coef=0.01,
        # learning_rate=lr_schedule,
        # clip_range=clip_schedule,
        # policy_kwargs=policy_kwargs,
    )
model.learn(total_timesteps=10e6)
model.save(f"{ALGO}_nakamoto_alpha_{ALPHA}")

env = gym.make("cpr-v0", spec=specs.nakamoto(alpha=ALPHA))
env = SparseRelativeRewardWrapper(env, alpha=ALPHA)
sum_rs = []
sum_attacker_rs = []
sum_defender_rs = []
relative_rs = []
actions = []
for i in tqdm(range(100)):
    done = False
    rs = []
    attacker_rs = []
    defender_rs = []
    obs = env.reset()
    while not done:
        action, _state = model.predict(obs, deterministic=True)
        obs, reward, done, info = env.step(action)
        rs.append(reward)
        attacker_rs.append(info["reward_attacker"])
        defender_rs.append(info["reward_defender"])
        actions.append(action)
    sum_rs.append(np.sum(rs))
    sum_attacker_rs.append(np.sum(attacker_rs))
    sum_defender_rs.append(np.sum(defender_rs))
    relative_rs.append(
        np.sum(attacker_rs) / (np.sum(defender_rs) + np.sum(attacker_rs))
    )
unique, unique_counts = np.unique(actions, return_counts=True)
unique_counts = dict(zip(unique, unique_counts))
print(f"Average reward: {np.mean(sum_rs)}, Std: {np.std(sum_rs)}")
print(f"Average relative reward: {np.mean(relative_rs)}, Std: {np.std(relative_rs)}")
print(f"Unique actions: {unique_counts}")
"""
Average reward: 46700.37878937397, Std: 176872.9906061578
Average relative reward: 0.48934404059594044, Std: 0.005359245320997703
Unique actions: {0: 489368, 1: 510632}
"""
