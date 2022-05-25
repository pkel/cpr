import sys, os, psutil, time
from uuid import uuid4

sys.path.append(os.getcwd())
import numpy as np
import pandas as pd

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
    SparseRelativeRewardWrapper,
    WastedBlocksRewardWrapper,
    AbsoluteRewardWrapper,
)
from rl.wrappers.honest_policy_wrapper import HonestPolicyWrapper

from rl.wrappers.decreasing_alpha_wrapper import AlphaScheduleWrapper
from rl.wrappers.illegal_move_wrapper import IllegalMoveWrapper

from stable_baselines3.common.callbacks import (
    BaseCallback,
    CheckpointCallback,
    EvalCallback,
    EventCallback,
    CallbackList,
)
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
        max_steps = config["STEPS_PER_ROLLOUT"] * 10
        max_time = config["STEPS_PER_ROLLOUT"]
    else:
        max_steps = config["STEPS_PER_ROLLOUT"]
        max_time = config["STEPS_PER_ROLLOUT"] * 10
    return gym.make(
        "cpr_gym:auto-v0",
        proto=proto,
        #  alpha=alpha,
        max_steps=max_steps,
        max_time=max_time,
        gamma=config["GAMMA"],
        defenders=config["DEFENDERS"],
        alpha_min=0.33,
        alpha_max=0.475,
        #  activation_delay=target,
    )


config = dict(
    PROTOCOL="nakamoto",
    K=10,
    ALGO="PPO",
    TOTAL_TIMESTEPS=1e7,
    STEPS_PER_ROLLOUT=2016,
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
    N_ENVS=psutil.cpu_count(),
)


class LogDaaBufferCallback(BaseCallback):
    def __init__(self, prefix="daa_buffer"):
        super().__init__()
        self.prefix = prefix

    def _on_step(self):
        data = pd.concat(self.training_env.get_attr("buf"))
        data["reward_per_alpha"] = data.reward
        data["reward"] = data.reward * data.alpha
        table = wandb.Table(
            data=data,
            columns=[
                "alpha",
                "activation_delay",
                "observed_block_interval",
                "reward_per_alpha",
                "reward",
            ],
        )
        rb = {
            f"{self.prefix}/reward": wandb.plot.line(table, "alpha", "reward"),
            f"{self.prefix}/reward_per_alpha": wandb.plot.line(
                table, "alpha", "reward_per_alpha"
            ),
            f"{self.prefix}/activation_delay": wandb.plot.line(
                table, "alpha", "activation_delay"
            ),
            f"{self.prefix}/observed_block_interval": wandb.plot.line(
                table, "alpha", "observed_block_interval"
            ),
        }
        # log
        wandb.log(rb)
        return True


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
        # wip logging
        for i in range(0, len(done)):
            if done[i]:
                l = {"episode/i": self.total_episodes + self.epoch_episodes}
                for k in [
                    "alpha",
                    "activation_delay",
                    "episode_reward",
                    "observed_runtime",
                    "observed_block_interval",
                    "daa_error",
                    "daa_extra_reward",
                    "daa_input_episodes",
                ]:
                    l[f"episode/{k}"] = info[i][k]
                wandb.log(l)
        return obs, reward, done, info


class OnRolloutStart(EventCallback):
    def __init__(self, callback: BaseCallback):
        super(OnRolloutStart, self).__init__(callback)

    def _on_rollout_start(self):
        return self._on_event()


class OnRolloutEnd(EventCallback):
    def __init__(self, callback: BaseCallback):
        super(OnRolloutEnd, self).__init__(callback)

    def _on_rollout_end(self):
        return self._on_event()


if __name__ == "__main__":
    wandb.init(project="dqn", entity="tailstorm", config=config)
    # config = wandb.config

    log_dir = f"saved_models/{wandb.run.id}"
    # log_dir = f"saved_models/test"

    os.makedirs(log_dir, exist_ok=True)

    def vec_env_fn():
        env = env_fn(0, 1, config)
        # env = AlphaScheduleWrapper(env, env_fn, config)
        # if config["USE_DAA"]:
        #     env = AbsoluteRewardWrapper(env)
        # else:
        #     env = WastedBlocksRewardWrapper(env)
        return env

    if config["N_ENVS"] > 1:
        env = SubprocVecEnv([vec_env_fn] * config["N_ENVS"])
    else:
        env = DummyVecEnv([vec_env_fn])
    env = VecMonitor(env)

    checkpoint_callback = CheckpointCallback(
        save_freq=1,
        save_path=log_dir,
    )

    eval_callback = EvalCallback(
        env,  # use training env for evaluation
        best_model_save_path=log_dir,
        log_path=log_dir,
        eval_freq=1,
        n_eval_episodes=128
        * config["N_ENVS"],  # flush daa ring buffers with deterministic actions
        deterministic=True,
        render=False,
    )

    rollout_start = OnRolloutStart(
        CallbackList(
            [
                eval_callback,
                LogDaaBufferCallback("daa_buffer_eval"),
                checkpoint_callback,
            ]
        )
    )
    rollout_end = OnRolloutEnd(LogDaaBufferCallback("daa_buffer_rollout"))
    wandb_callback = WandbCallback(gradient_save_freq=10000)

    callback = CallbackList([wandb_callback, rollout_start, rollout_end])

    #  env = VecWandbLogger(env)

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
        callback=callback,
    )
    model.save(os.path.join(log_dir, f"{config['ALGO']}_bkll"))
