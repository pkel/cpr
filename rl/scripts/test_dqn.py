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
    STEPS_PER_ROLLOUT=250,
    LR_SCHEDULE=lr_schedule,
    BATCH_SIZE=64000,
    ALPHA_SCHEDULE_CUTOFF=0,
    LAYER_SIZE=100,
    N_LAYERS=2,
)
ALPHA = 0.35


def env_fn(alpha):
    return gym.make(
        "cpr-v0",
        spec=specs.nakamoto(
            alpha=alpha, n_steps=config["STEPS_PER_ROLLOUT"], gamma=0, defenders=1
        ),
    )


def alpha_schedule(step):
    progress = step / (config["TOTAL_TIMESTEPS"] / config["STEPS_PER_ROLLOUT"])
    if progress >= config["ALPHA_SCHEDULE_CUTOFF"]:
        alpha = np.random.normal(0.3, 0.1)
        alpha = min(alpha, 0.49)
        alpha = max(alpha, 0.25)
    else:
        alpha = 0.25 * progress + 0.5 * (1 - progress)
    return ALPHA
    return alpha


env = env_fn(ALPHA)
env = AlphaScheduleWrapper(env, env_fn, alpha_schedule)
env = SparseRelativeRewardWrapper(env, relative=False)

obs = env.reset()
# while True:
#     action = env.action_space.sample()
#     obs, reward, done, info = env.step(action)
#     env.render()
#     if done:
#         obs = env.reset()
#         print(env.alpha)

model = PPO.load(f"saved_models/best_model.zip", env=env)
test_rewards = []
for _ in tqdm(range(100)):
    done = False
    obs = env.reset()
    actions = []
    while not done:
        action, _state = model.predict(np.array(obs))
        next_obs, reward, done, info = env.step(action)
        obs = next_obs
        actions.append(action)
    # print(actions)
    test_rewards.append(reward)
print(test_rewards)

print(np.mean(test_rewards))
# Average reward: 48464.505278942146, Std: 111065.6392517102

"""
Sum of rewards: 176471.09737369884
Sum of Attacker rewards: 4948.0
Sum of Defender rewards: 5052.0
Actions: {0: 4877, 1: 5123}
"""
