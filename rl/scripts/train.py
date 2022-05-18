import sys, os
from uuid import uuid4


sys.path.append(os.getcwd())
import numpy as np

import gym
from cpr_gym import protocols
from stable_baselines3 import A2C, PPO, DQN
from stable_baselines3.ppo.policies import MlpPolicy
from tqdm import tqdm
import torch
from stable_baselines3.common.env_util import make_vec_env
from rl.wrappers.exploration_reward_wrapper import ExplorationRewardWrapper
from rl.wrappers.excess_reward_wrapper import (
    RelativeRewardWrapper,
    SparseRelativeRewardWrapper,
    WastedBlocksRewardWrapper,
    AbsoluteRewardWrapper,
)
from rl.wrappers.honest_policy_wrapper import HonestPolicyWrapper

from rl.wrappers.decreasing_alpha_wrapper import AlphaScheduleWrapper
from rl.wrappers.illegal_move_wrapper import IllegalMoveWrapper

from stable_baselines3.common.callbacks import BaseCallback
from stable_baselines3.common.results_plotter import load_results, ts2xy, plot_results
from stable_baselines3.common.monitor import Monitor

import wandb
from wandb.sdk.lib import telemetry as wb_telemetry
from wandb.integration.sb3 import WandbCallback


class WandbCallback(BaseCallback):
    """Log SB3 experiments to Weights and Biases
        - Added model tracking and uploading
        - Added complete hyperparameters recording
        - Added gradient logging
        - Note that `wandb.init(...)` must be called before the WandbCallback can be used

    Args:
        verbose: The verbosity of sb3 output
        model_save_path: Path to the folder where the model will be saved, The default value is `None` so the model is not logged
        model_save_freq: Frequency to save the model
        gradient_save_freq: Frequency to log gradient. The default value is 0 so the gradients are not logged
    """

    def __init__(
        self,
        verbose: int = 0,
        model_save_path: str = None,
        model_save_freq: int = 0,
        gradient_save_freq: int = 0,
    ):
        super(WandbCallback, self).__init__(verbose)
        if wandb.run is None:
            raise wandb.Error("You must call wandb.init() before WandbCallback()")
        with wb_telemetry.context() as tel:
            tel.feature.sb3 = True
        self.model_save_freq = model_save_freq
        self.model_save_path = model_save_path
        self.gradient_save_freq = gradient_save_freq
        self.best_mean_reward = -np.inf

        # Create folder if needed
        if self.model_save_path is not None:
            os.makedirs(self.model_save_path, exist_ok=True)
            self.path = os.path.join(self.model_save_path, "model.zip")
        else:
            assert (
                self.model_save_freq == 0
            ), "to use the `model_save_freq` you have to set the `model_save_path` parameter"

    def _init_callback(self) -> None:
        d = {}
        if "algo" not in d:
            d["algo"] = type(self.model).__name__
        for key in self.model.__dict__:
            if key in wandb.config:
                continue
            if type(self.model.__dict__[key]) in [float, int, str]:
                d[key] = self.model.__dict__[key]
            else:
                d[key] = str(self.model.__dict__[key])
        if self.gradient_save_freq > 0:
            wandb.watch(self.model.policy, log_freq=self.gradient_save_freq, log="all")
        wandb.config.setdefaults(d)

    def _on_step(self) -> bool:
        if self.model_save_freq > 0:
            if self.model_save_path is not None:
                if self.n_calls % self.model_save_freq == 0:
                    difficulties = self.locals["infos"][0]["difficulties"]
                    if difficulties is not None:
                        for key, value in difficulties.items():
                            wandb.log({f"difficulty/{key}": value})
                    self.save_model()
                    x, y = ts2xy(load_results(self.model_save_path), "timesteps")
                    if len(x) > 0:
                        # Mean training reward over the last 100 episodes
                        mean_reward = np.mean(y[-5000:])
                        wandb.log({"mean_reward": mean_reward})

                        # New best model, you could save the agent here
                        if mean_reward > self.best_mean_reward:
                            self.best_mean_reward = mean_reward
                            # Example for saving best model

                            path = os.path.join(self.model_save_path, "best_model.zip")
                            self.model.save(path)
                            wandb.save(path, base_path=self.model_save_path)
        return True

    def _on_training_end(self) -> None:
        if self.model_save_path is not None:
            self.save_model()

    def save_model(self) -> None:
        self.model.save(self.path)
        wandb.save(self.path, base_path=self.model_save_path)


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
        max_steps = config["STEPS_PER_ROLLOUT"] * 10
        max_time = config["STEPS_PER_ROLLOUT"]
    else:
        max_steps = config["STEPS_PER_ROLLOUT"]
        max_time = config["STEPS_PER_ROLLOUT"] * 10
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
    GAMMA=0,
    DEFENDERS=1,
    ACTIVATION_DELAY=1,
)


wandb.init(project="dqn", entity="tailstorm", config=config)
# config = wandb.config

log_dir = f"saved_models/{wandb.run.id}"
# log_dir = f"saved_models/test"

os.makedirs(log_dir, exist_ok=True)
env = env_fn(0, 1, config)
env = AlphaScheduleWrapper(env, env_fn, config)
if config["USE_DAA"]:
    env = AbsoluteRewardWrapper(env)
else:
    env = WastedBlocksRewardWrapper(env)

env = Monitor(env, log_dir)
print(env.action_space)
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
model.learn(
    total_timesteps=config["TOTAL_TIMESTEPS"],
    callback=WandbCallback(
        gradient_save_freq=1000,
        model_save_path=log_dir,
        model_save_freq=1000,
        verbose=0,
    ),
)
model.save(os.path.join(log_dir, f"{config['ALGO']}_bkll"))
