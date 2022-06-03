import sys, os, psutil, time
from uuid import uuid4

sys.path.append(os.getcwd())
import numpy as np

import gym
from cpr_gym import protocols
import stable_baselines3
from stable_baselines3 import A2C, PPO, DQN
from stable_baselines3.ppo.policies import MlpPolicy
from stable_baselines3.common.vec_env import DummyVecEnv, SubprocVecEnv, VecMonitor
from stable_baselines3.common.vec_env.base_vec_env import (
    VecEnv,
    VecEnvWrapper,
    VecEnvStepReturn,
)
from tqdm import tqdm
import torch
from rl.wrappers.exploration_reward_wrapper import ExplorationRewardWrapper
from rl.wrappers.excess_reward_wrapper import (
    RelativeRewardWrapper,
    SparseDaaRewardWrapper,
    SparseRelativeRewardWrapper,
    WastedBlocksRewardWrapper,
    AbsoluteRewardWrapper,
)
from rl.wrappers import ReleaseOnDoneWrapper
from rl.wrappers.honest_policy_wrapper import HonestPolicyWrapper

from rl.wrappers.decreasing_alpha_wrapper import AlphaScheduleWrapper
from rl.wrappers.illegal_move_wrapper import IllegalMoveWrapper

from stable_baselines3.common.callbacks import BaseCallback
from stable_baselines3.common.results_plotter import load_results, ts2xy, plot_results
from stable_baselines3.common.monitor import Monitor

import wandb
from wandb.sdk.lib import telemetry as wb_telemetry
from wandb.integration.sb3 import WandbCallback


def lr_schedule(remaining):
    return config["STARTING_LR"] * remaining + config["ENDING_LR"] * (1 - remaining)


def clip_schedule(remaining):
    return 0.1


def env_fn(alpha, target, config):
    if config["PROTOCOL"] == "nakamoto":
        proto = protocols.nakamoto()
    elif config["PROTOCOL"] == "bk_ll":
        proto = protocols.bk_ll(k=config["K"])

    if config["USE_DAA"]:
        max_steps = config["STEPS_PER_ROLLOUT"] * 1000
        max_time = config["STEPS_PER_ROLLOUT"]
    else:
        max_steps = config["STEPS_PER_ROLLOUT"]
        max_time = config["STEPS_PER_ROLLOUT"] * 1000
    return gym.make(
        "cpr-v0",
        proto=proto,
        alpha=alpha,
        max_steps=max_steps,
        max_time=max_time,
        gamma=config["GAMMA"],
        defenders=config["DEFENDERS"],
        activation_delay=target,
    )


config = dict(
    PROTOCOL="nakamoto",
    K=10,
    ALGO="PPO",
    TOTAL_TIMESTEPS=10e7,
    STEPS_PER_ROLLOUT=200,
    STARTING_LR=10e-5,
    ENDING_LR=10e-6,
    BATCH_SIZE=2048,
    ALPHA_SCHEDULE_CUTOFF=0,
    LAYER_SIZE=100,
    N_LAYERS=2,
    N_STEPS_MULTIPLE=10,
    HONEST_STEPS_FRACTION=0.1,
    STARTING_EPS=0.99,
    ENDING_EPS=0.01,
    ALPHA_SCHEDULE=[
        0.15,
        0.25,
        1 / 3.0,
        0.35,
        0.375,
        0.4,
        0.425,
        0.45,
        0.475,
    ],
    USE_DAA=True,
    DAA_METHOD="sparse",
    GAMMA=0,
    DEFENDERS=1,
    ACTIVATION_DELAY=1,
    N_ENVS=16,
)


class VecWandbLogger(VecEnvWrapper):
    def __init__(
        self,
        venv: VecEnv,
        every: int = 10000,
    ):
        self.every = every
        self.epoch_episodes = 0
        self.epoch_vec_steps = 0
        self.total_episodes = 0
        self.total_vec_steps = 0
        self.last_epoch = time.time()
        super().__init__(venv=venv)

    def reset(self) -> np.ndarray:
        obs = self.venv.reset()
        return obs

    def step_async(self, actions: np.ndarray) -> None:
        self.venv.step_async(actions)

    def step_wait(
        self,
    ) -> VecEnvStepReturn:
        obs, reward, done, info = self.venv.step_wait()
        # count a few things
        self.epoch_vec_steps += 1
        for b in done:
            if b:
                self.epoch_episodes += 1
        # regular logging
        if self.epoch_vec_steps >= self.every:
            # performance
            n_envs = config["N_ENVS"]
            now = time.time()
            seconds = now - self.last_epoch
            performance = {
                "performance/steps_per_second": self.epoch_vec_steps * n_envs / seconds,
                "performance/epochs_per_second": self.epoch_episodes / seconds,
            }
            # reset counters
            self.total_vec_steps += self.epoch_vec_steps
            self.total_episodes += self.epoch_episodes
            self.epoch_vec_steps = 0
            self.epoch_episodes = 0
            self.last_epoch = now
            # progress
            progress = {
                "progress/steps": self.total_vec_steps * n_envs,
                "progress/episodes": self.total_episodes,
            }
            # daa difficulties
            try:
                d = [x["difficulties"] for x in info]
                d = {
                    f"difficulty/α={a:.2f}": np.mean([x[a] for x in d])
                    for a in d[0].keys()
                }
            except:
                d = {"difficulty": None}
            # mean rewards
            r = {}
            for i in self.venv.get_attr("rolling_reward"):
                for alpha, value in i.items():
                    if alpha in r.keys():
                        r[alpha].extend(value)
                    else:
                        r[alpha] = value
            r = {f"reward/α={a:.2f}": np.mean(l) for a, l in r.items()}
            # log
            wandb.log(progress | performance | d | r)
        return obs, reward, done, info


if __name__ == "__main__":
    wandb.init(project="dqn", entity="tailstorm", config=config)
    # config = wandb.config

    log_dir = f"saved_models/{wandb.run.id}"
    # log_dir = f"saved_models/test"

    os.makedirs(log_dir, exist_ok=True)

    def vec_env_fn():
        env = env_fn(0, 1, config)
        env = AlphaScheduleWrapper(env, env_fn, config)
        env = ReleaseOnDoneWrapper(env)
        if config["USE_DAA"]:
            if config["DAA_METHOD"] == "sparse":
                env = SparseDaaRewardWrapper(env)
            else:
                env = AbsoluteRewardWrapper(env)
        else:
            env = WastedBlocksRewardWrapper(env)
        return env

    if config["N_ENVS"] > 1:
        env = SubprocVecEnv([vec_env_fn] * config["N_ENVS"])
    else:
        env = DummyVecEnv([vec_env_fn])
    env = VecMonitor(env)
    env = VecWandbLogger(env)
    print(env.action_space)
    print(env.observation_space)
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
            gamma=0.999,
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

    model.learn(
        total_timesteps=config["TOTAL_TIMESTEPS"],
        callback=WandbCallback(
            gradient_save_freq=10000,
            model_save_path=log_dir,
            model_save_freq=10000,
            verbose=0,
        ),
    )
    model.save(os.path.join(log_dir, f"{config['ALGO']}_{config['PROTOCOL']}"))
