import sys, os
from uuid import uuid4


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
from rl.wrappers.honest_policy_wrapper import HonestPolicyWrapper

from rl.wrappers.decreasing_alpha_wrapper import AlphaScheduleWrapper
from rl.wrappers.illegal_move_wrapper import IllegalMoveWrapper

from stable_baselines3.common.callbacks import BaseCallback
from stable_baselines3.common.results_plotter import load_results, ts2xy, plot_results
from stable_baselines3.common.monitor import Monitor

import wandb


class LoggingCallback(BaseCallback):
    """
    Custom callback for plotting additional values in tensorboard.
    """

    def __init__(self, verbose=0):
        super(LoggingCallback, self).__init__(verbose)

    def _on_step(self) -> bool:
        # Log scalar value (here a random variable)
        x, y = ts2xy(load_results(log_dir), "timesteps")
        mean_reward = np.mean(y[-100:])
        wandb.log({"mean_reward": mean_reward})
        return True


def lr_schedule(remaining):
    return config["STARTING_LR"] * remaining + config["ENDING_LR"] * (1 - remaining)


def clip_schedule(remaining):
    return 0.1


def env_fn(alpha):
    return gym.make(
        "cpr-v0",
        spec=specs.nakamoto(
            alpha=alpha, n_steps=config["STEPS_PER_ROLLOUT"], gamma=0, defenders=1
        ),
    )


config = dict(
    # ALPHA=0.35,
    ALGO="DQN",
    TOTAL_TIMESTEPS=10e6,
    STEPS_PER_ROLLOUT=20,
    STARTING_LR=10e-4,
    ENDING_LR=10e-7,
    BATCH_SIZE=100000,
    ALPHA_SCHEDULE_CUTOFF=0,
    LAYER_SIZE=100,
    N_LAYERS=2,
    N_STEPS_MULTIPLE=10,
    HONEST_STEPS_FRACTION=0.1,
    STARTING_EPS=0.5,
    ENDING_EPS=0.01,
)


wandb.init(project="dqn", entity="bglick13", config=config)
config = wandb.config


def alpha_schedule(step):
    progress = step / (config["TOTAL_TIMESTEPS"] / config["STEPS_PER_ROLLOUT"])
    if progress >= config["ALPHA_SCHEDULE_CUTOFF"]:
        alpha = np.random.normal(0.3, 0.15)
        alpha = min(alpha, 0.49)
        alpha = max(alpha, 0.05)
    else:
        alpha = 0.25 * progress + 0.5 * (1 - progress)
    return alpha


log_dir = wandb.run.dir
# os.makedirs(log_dir, exist_ok=True)
env = gym.make(
    "cpr-v0",
    spec=specs.nakamoto(
        alpha=0, n_steps=config["STEPS_PER_ROLLOUT"], gamma=0, defenders=1
    ),
)
env = AlphaScheduleWrapper(env, env_fn, alpha_schedule)
env = SparseRelativeRewardWrapper(env)
env = IllegalMoveWrapper(env)
# env = HonestPolicyWrapper(
#     env, int(config["HONEST_STEPS_FRACTION"] * config["TOTAL_TIMESTEPS"])
# )
env = Monitor(env, log_dir)

if config["ALGO"] == "PPO":
    policy_kwargs = dict(
        activation_fn=torch.nn.ReLU,
        net_arch=[
            dict(
                pi=[config["LAYER_SIZE"]] * config["N_LAYERS"],
                vf=[config["LAYER_SIZE"]] * config["N_LAYERS"],
            )
        ],
    )
    model = PPO(
        "MlpPolicy",
        env,
        verbose=1,
        batch_size=config["BATCH_SIZE"],
        n_steps=config["BATCH_SIZE"] * config["N_STEPS_MULTIPLE"],
        clip_range=0.1,
        # ent_coef=0.01,
        learning_rate=lr_schedule,
        # clip_range=clip_schedule,
        policy_kwargs=policy_kwargs,
    )
elif config["ALGO"] == "DQN":
    policy_kwargs = dict(
        activation_fn=torch.nn.ReLU,
        net_arch=[config["LAYER_SIZE"]] * config["N_LAYERS"],
    )
    model = DQN(
        "MlpPolicy",
        env,
        verbose=1,
        batch_size=config["BATCH_SIZE"],
        # clip_range=0.1,
        # ent_coef=0.01,
        learning_rate=lr_schedule,
        # clip_range=clip_schedule,
        policy_kwargs=policy_kwargs,
        exploration_initial_eps=config["STARTING_EPS"],
        exploration_final_eps=config["ENDING_EPS"],
    )
    # replay_buffer = model.replay_buffer
    # actions = []
    # new_obs = []
    # rewards = []
    # dones = []
    # infos = []
    # i = 0
    # while i < model.learning_starts * config["N_STEPS_MULTIPLE"]:
    #     obs = env.reset()
    #     done = False
    #     while not done:
    #         actions = []
    #         new_obs = []
    #         rewards = []
    #         dones = []
    #         infos = []
    #         if obs[1] > obs[0]:
    #             action = 1
    #         elif obs[1] < obs[0]:
    #             action = 0
    #         else:
    #             action = 3
    #         obs, reward, done, info = env.step(action)
    #         actions.append(action)
    #         new_obs.append(obs)
    #         rewards.append(reward)
    #         dones.append(done)
    #         infos.append(info)
    #     actions = np.array(actions)
    #     new_obs = np.array(new_obs)
    #     rewards = np.array(rewards)
    #     dones = np.array(dones)
    #     infos = np.array(infos)

    #     model._store_transition(replay_buffer, actions, new_obs, rewards, dones, infos)
    #     i += 1
    # model.load_replay_buffer()
model.learn(total_timesteps=config["TOTAL_TIMESTEPS"], callback=LoggingCallback())
model.save(os.path.join(wandb.run.dir, f"{config['ALGO']}_nakamoto"))

# env = gym.make(
#     "cpr-v0",
#     spec=specs.nakamoto(alpha=config["ALPHA"], n_steps=config["STEPS_PER_ROLLOUT"]),
# )
# env = SparseRelativeRewardWrapper(env, alpha=config["ALPHA"])
# sum_rs = []
# sum_attacker_rs = []
# sum_defender_rs = []
# relative_rs = []
# actions = []
# for i in tqdm(range(100)):
#     done = False
#     rs = []
#     attacker_rs = []
#     defender_rs = []
#     obs = env.reset()
#     while not done:
#         action, _state = model.predict(obs, deterministic=True)
#         obs, reward, done, info = env.step(action)
#         rs.append(reward)
#         attacker_rs.append(info["reward_attacker"])
#         defender_rs.append(info["reward_defender"])
#         actions.append(action)
#     sum_rs.append(np.sum(rs))
#     sum_attacker_rs.append(np.sum(attacker_rs))
#     sum_defender_rs.append(np.sum(defender_rs))
#     relative_rs.append(
#         np.sum(attacker_rs) / (np.sum(defender_rs) + np.sum(attacker_rs))
#     )
# unique, unique_counts = np.unique(actions, return_counts=True)
# unique_counts = dict(zip(unique, unique_counts))
# print(f"Average reward: {np.mean(sum_rs)}, Std: {np.std(sum_rs)}")
# print(f"Average relative reward: {np.mean(relative_rs)}, Std: {np.std(relative_rs)}")
# print(f"Unique actions: {unique_counts}")
"""
Average reward: 46700.37878937397, Std: 176872.9906061578
Average relative reward: 0.48934404059594044, Std: 0.005359245320997703
Unique actions: {0: 489368, 1: 510632}
"""
