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
    WastedBlocksRewardWrapper,
)
from rl.wrappers.honest_policy_wrapper import HonestPolicyWrapper

from rl.wrappers.decreasing_alpha_wrapper import AlphaScheduleWrapper
from rl.wrappers.illegal_move_wrapper import IllegalMoveWrapper

from stable_baselines3.common.callbacks import BaseCallback
from stable_baselines3.common.results_plotter import load_results, ts2xy, plot_results
from stable_baselines3.common.monitor import Monitor

import wandb
from wandb.sdk.lib import telemetry as wb_telemetry


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
                    self.save_model()
            x, y = ts2xy(load_results(self.log_dir), "timesteps")
            if len(x) > 0:
                # Mean training reward over the last 100 episodes
                mean_reward = np.mean(y[-1000:])
                # wandb.log({"mean_reward": mean_reward})

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


class SaveOnBestTrainingRewardCallback(BaseCallback):
    """
    Callback for saving a model (the check is done every ``check_freq`` steps)
    based on the training reward (in practice, we recommend using ``EvalCallback``).

    :param check_freq:
    :param log_dir: Path to the folder where the model will be saved.
      It must contains the file created by the ``Monitor`` wrapper.
    :param verbose: Verbosity level.
    """

    def __init__(self, check_freq: int, log_dir: str, verbose: int = 1):
        super(SaveOnBestTrainingRewardCallback, self).__init__(verbose)
        self.check_freq = check_freq
        self.log_dir = log_dir
        self.save_path = os.path.join(log_dir, "best_model")
        self.best_mean_reward = -np.inf

    def _init_callback(self) -> None:
        # Create folder if needed
        if self.save_path is not None:
            os.makedirs(self.save_path, exist_ok=True)

    def _on_step(self) -> bool:
        if self.n_calls % self.check_freq == 0:

            # Retrieve training reward
            x, y = ts2xy(load_results(self.log_dir), "timesteps")
            if len(x) > 0:
                # Mean training reward over the last 100 episodes
                mean_reward = np.mean(y[-1000:])
                # wandb.log({"mean_reward": mean_reward})

                if self.verbose > 0:
                    print(f"Num timesteps: {self.num_timesteps}")
                    print(
                        f"Best mean reward: {self.best_mean_reward:.2f} - Last mean reward per episode: {mean_reward:.2f}"
                    )

                # New best model, you could save the agent here
                if mean_reward > self.best_mean_reward:
                    self.best_mean_reward = mean_reward
                    # Example for saving best model
                    if self.verbose > 0:
                        print(f"Saving new best model to {self.save_path}")
                    self.model.save(self.save_path)

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
    ALGO="PPO",
    TOTAL_TIMESTEPS=10e6,
    STEPS_PER_ROLLOUT=250,
    STARTING_LR=10e-4,
    ENDING_LR=10e-7,
    BATCH_SIZE=2048,
    ALPHA_SCHEDULE_CUTOFF=0,
    LAYER_SIZE=100,
    N_LAYERS=2,
    N_STEPS_MULTIPLE=10,
    HONEST_STEPS_FRACTION=0.1,
    STARTING_EPS=0.99,
    ENDING_EPS=0.01,
)


wandb.init(project="dqn", entity="bglick13", config=config)
# config = wandb.config


def alpha_schedule(step):
    progress = step / (config["TOTAL_TIMESTEPS"] / config["STEPS_PER_ROLLOUT"])
    if progress >= config["ALPHA_SCHEDULE_CUTOFF"]:
        alpha = np.random.normal(0.3, 0.15)
        alpha = min(alpha, 0.49)
        alpha = max(alpha, 0.05)
    else:
        alpha = 0.25 * progress + 0.5 * (1 - progress)
    return 0.35
    return alpha


log_dir = f"saved_models/{wandb.run.id}"
# log_dir = f"saved_models/test"

os.makedirs(log_dir, exist_ok=True)
env = gym.make(
    "cpr-v0",
    spec=specs.nakamoto(
        alpha=0, n_steps=config["STEPS_PER_ROLLOUT"], gamma=0, defenders=1
    ),
)
env = AlphaScheduleWrapper(env, env_fn, alpha_schedule)
env = WastedBlocksRewardWrapper(env)
# env = IllegalMoveWrapper(env)
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
model.learn(
    total_timesteps=config["TOTAL_TIMESTEPS"],
    callback=WandbCallback(
        gradient_save_freq=1000,
        model_save_path=f"saved_models/{wandb.run.id}",
        model_save_freq=1000,
        verbose=0,
    ),
)
model.save(os.path.join(log_dir, f"{config['ALGO']}_nakamoto"))

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
