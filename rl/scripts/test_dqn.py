import sys, os

sys.path.append(os.getcwd())

import numpy as np

import gym
from cpr_gym import specs, engine
from stable_baselines3 import A2C, PPO, DQN
from tqdm import tqdm
import matplotlib.pyplot as plt
import seaborn as sns
from rl.wrappers.exploration_reward_wrapper import ExplorationRewardWrapper
from rl.wrappers.excess_reward_wrapper import (
    RelativeRewardWrapper,
    SparseRelativeRewardWrapper,
)
from rl.wrappers.decreasing_alpha_wrapper import AlphaScheduleWrapper


def lr_schedule(remaining):
    return 10e-5 * remaining + 10e-7 * (1 - remaining)


config = dict(
    # ALPHA=0.35,
    ALGO="DQN",
    TOTAL_TIMESTEPS=10e6,
    STEPS_PER_ROLLOUT=10,
    LR_SCHEDULE=lr_schedule,
    BATCH_SIZE=64000,
    ALPHA_SCHEDULE_CUTOFF=0,
    LAYER_SIZE=100,
    N_LAYERS=2,
)
ALPHA = 0.25


def env_fn(alpha):
    return gym.make(
        "cpr-v0", spec=specs.nakamoto(alpha=alpha, n_steps=config["STEPS_PER_ROLLOUT"])
    )


def alpha_schedule(step):
    progress = step / (config["TOTAL_TIMESTEPS"] / config["STEPS_PER_ROLLOUT"])
    if progress >= config["ALPHA_SCHEDULE_CUTOFF"]:
        alpha = np.random.normal(0.3, 0.1)
        alpha = min(alpha, 0.49)
        alpha = max(alpha, 0.25)
    else:
        alpha = 0.25 * progress + 0.5 * (1 - progress)
    return alpha


env = gym.make("cpr-v0", spec=specs.nakamoto(alpha=ALPHA))
env = AlphaScheduleWrapper(env, env_fn, alpha_schedule)
env = SparseRelativeRewardWrapper(env)

obs = env.reset()
while True:
    action = env.action_space.sample()
    obs, reward, done, info = env.step(action)
    env.render()
    if done:
        obs = env.reset()
        print(env.alpha)

model = PPO.load(f"ppo_nakamoto_alpha_{ALPHA}")

sum_rs = []
sum_attacker_rs = []
sum_defender_rs = []
relative_rs = []
actions = []
_is = []
for _ in tqdm(range(100)):
    done = False
    rs = []
    attacker_rs = []
    defender_rs = []
    _actions = []
    tds = []
    i = 0
    obs = env.reset()
    while not done:
        action, _state = model.predict(np.array(obs), deterministic=True)
        next_obs, reward, done, info = env.step(action)
        rs.append(reward)
        attacker_rs.append(info["reward_attacker"])
        defender_rs.append(info["reward_defender"])
        tds.append(info["reward_time_elapsed"])
        _actions.append(action)
        i += 1
        obs = next_obs
    _is.append(i)
    actions += _actions
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
print(f"Average steps: {np.mean(_is)}, Std: {np.std(_is)}")
fig, ax = plt.subplots()
sns.distplot(sum_rs, ax=ax)
plt.show()

# Average reward: 48464.505278942146, Std: 111065.6392517102

"""
Sum of rewards: 176471.09737369884
Sum of Attacker rewards: 4948.0
Sum of Defender rewards: 5052.0
Actions: {0: 4877, 1: 5123}
"""
