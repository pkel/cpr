import configparser
import cpr_gym
import cpr_gym.wrappers
import gym
import os
import pprint
import psutil
import random
import stable_baselines3
import torch
import utils
import wandb
import wandb.integration.sb3

os.chdir(os.path.dirname(__file__))

###
# load config
###

ini = configparser.ConfigParser()
ini.read_dict(dict(main=dict(n_envs=psutil.cpu_count())))
ini.read(["defaults.ini", "config.ini"])
config = dict()
for s in ini.sections():
    config[s] = dict()
    for k in ini.options(s):
        config[s][k] = ini.get(s, k)


def cast(s, k, f):
    if k in config[s].keys():
        config[s][k] = f(config[s][k])


cast("main", "n_envs", int)
cast("main", "total_timesteps", float)
cast("main", "alpha_min", float)
cast("main", "alpha_max", float)
cast("main", "episode_len", int)
cast("env_args", "activation_delay", float)
cast("env_args", "alpha", float)
cast("env_args", "gamma", float)
cast("env_args", "defenders", int)
cast("env_args", "max_steps", float)
cast("protocol_args", "k", int)
cast("ppo", "batch_size", int)
cast("ppo", "gamma", float)
cast("ppo", "n_steps_multiple", int)
cast("ppo", "n_layers", int)
cast("ppo", "layer_size", int)
cast("ppo", "starting_lr", float)
cast("ppo", "ending_lr", float)


###
# W&B init
###

if "tags" in config["wandb"].keys():
    wandb_tags = [str(t).strip() for t in str(config["wandb"]["tags"]).split(",")]
else:
    wandb_tags = []

config.pop("wandb", None)
config["engine_version"] = cpr_gym.engine.cpr_lib_version

if __name__ == "__main__":
    wandb.init(
        project="dqn",
        entity="tailstorm",
        tags=["ppo"] + wandb_tags,
        config=dict(config=config),
    )

    print("## Configuration ##")
    pprint.pprint(config)

###
# env
###


def env_fn():
    protocol_fn = getattr(cpr_gym.protocols, config["main"]["protocol"])
    protocol_args = config["protocol_args"]
    env_args = config["env_args"]

    if config["main"]["reward"] != "dense_per_block":
        if "max_steps" not in env_args.keys():
            env_args["max_steps"] = config["main"]["episode_len"]

    env = gym.make(
        config["main"]["env"], proto=protocol_fn(**protocol_args), **env_args
    )

    reward = dict(
        sparse_relative=cpr_gym.wrappers.SparseRelativeRewardWrapper,
        sparse_per_block=cpr_gym.wrappers.SparseRewardPerBlockWrapper,
        dense_per_block=lambda env: cpr_gym.wrappers.DenseRewardPerBlockWrapper(
            env, config["main"]["episode_len"]
        ),
    )

    env = reward[config["main"]["reward"]](env)

    if "alpha_schedule" in config["main"].keys():
        alpha_schedule = [float(a) for a in config["main"]["alpha_schedule"].split(",")]
    elif "alpha_min" in config["main"].keys():

        def alpha_schedule():
            return random.uniform(
                config["main"]["alpha_min"], config["main"]["alpha_max"]
            )

    else:

        def alpha_schedule():
            return config["main"]["alpha"]

    env = cpr_gym.wrappers.AlphaScheduleWrapper(env, alpha_schedule)

    return env


if __name__ == "__main__":
    print("## Environment (not vectorized) ##")
    env_fn().render()


def venv_fn():
    if config["main"]["n_envs"] > 1:
        env = stable_baselines3.common.vec_env.SubprocVecEnv(
            [env_fn] * config["main"]["n_envs"]
        )
    else:
        env = stable_baselines3.common.vec_env.DummyVecEnv([env_fn])

    env = stable_baselines3.common.vec_env.VecMonitor(env)
    return env


###
# Training
###

if __name__ == "__main__":
    print("## Training ##")

    def lr_schedule(remaining):
        return config["ppo"]["starting_lr"] * remaining + config["ppo"]["ending_lr"] * (
            1 - remaining
        )

    log_dir = f"saved_models/ppo-{wandb.run.id}"

    vec_steps_per_rollout = (
        config["ppo"]["batch_size"] * config["ppo"]["n_steps_multiple"]
    )
    # rollout buffer is this time n_envs

    model = stable_baselines3.PPO(
        "MlpPolicy",
        env=venv_fn(),
        verbose=1,
        batch_size=config["ppo"]["batch_size"],
        gamma=config["ppo"]["gamma"],
        n_steps=vec_steps_per_rollout,
        clip_range=0.1,
        # ent_coef=0.01,
        learning_rate=lr_schedule,
        # clip_range=clip_schedule,
        policy_kwargs=dict(
            activation_fn=torch.nn.ReLU,
            net_arch=[
                dict(
                    pi=[config["ppo"]["layer_size"]] * config["ppo"]["n_layers"],
                    vf=[config["ppo"]["layer_size"]] * config["ppo"]["n_layers"],
                )
            ],
        ),
    )

    utils.setWandbLogger(model)

    model.learn(
        total_timesteps=config["main"]["total_timesteps"],
        callback=[
            wandb.integration.sb3.WandbCallback(
                gradient_save_freq=vec_steps_per_rollout,
                model_save_path=log_dir,
                model_save_freq=vec_steps_per_rollout,
                verbose=0,
            ),
            stable_baselines3.common.callbacks.EvalCallback(
                venv_fn(),
                best_model_save_path=log_dir,
                log_path=log_dir,
                eval_freq=vec_steps_per_rollout,
                n_eval_episodes=128,
                deterministic=True,
                render=False,
            ),
        ],
    )

    model.save(log_dir)
