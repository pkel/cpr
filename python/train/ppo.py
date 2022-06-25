import configparser
import cpr_gym
import cpr_gym.wrappers
import gym
import itertools
import json
import numpy
import os
import pandas
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
cast("eval", "alpha_step", float)
cast("eval", "episodes_per_alpha_per_env", int)
cast("eval", "recorder_multiple", int)
cast("eval", "report_alpha", int)

###
# Analyse config
###

if "tags" in config["wandb"].keys():
    config["wandb"]["tags"] = [
        str(t).strip() for t in str(config["wandb"]["tags"]).split(",")
    ] + ["ppo"]
else:
    config["wandb"]["tags"] = ["ppo"]


def alpha_schedule(eval=False):
    info = dict()
    if "alpha_schedule" in config["main"].keys():
        alphas = [float(a) for a in config["main"]["alpha_schedule"].split(",")]

        if eval:
            info["n_alphas"] = len(alphas)
            info["range"] = False
            alpha_schedule = alphas

        def alpha_schedule():
            return random.choice(alphas)

    elif "alpha_min" in config["main"].keys():
        if eval:
            alpha_schedule = numpy.arange(
                config["main"]["alpha_min"],
                numpy.nextafter(config["main"]["alpha_max"], 1),
                config["eval"]["alpha_step"],
            )
            info["n_alphas"] = numpy.size(alpha_schedule)
            info["range"] = True
        else:

            def alpha_schedule():
                return random.uniform(
                    config["main"]["alpha_min"], config["main"]["alpha_max"]
                )

    else:
        if eval:
            info["n_alphas"] = 1
            info["range"] = False

        if "alpha" in config["main"].keys():
            alpha_schedule = [config["main"]["alpha"]]
        else:
            alpha_schedule = [0.33]

    return alpha_schedule, info


info = dict()
info["engine_version"] = cpr_gym.engine.cpr_lib_version
info["episode_n_steps"] = config["main"]["episode_len"]
info["rollout_n_steps"] = (
    config["ppo"]["batch_size"]
    * config["ppo"]["n_steps_multiple"]
    * config["main"]["n_envs"]
)
info["rollout_n_episodes"] = info["rollout_n_steps"] / config["main"]["episode_len"]
info["rollout_n_batches"] = info["rollout_n_steps"] / config["ppo"]["batch_size"]
info["batch_n_steps"] = config["ppo"]["batch_size"]
info["batch_n_episodes"] = config["ppo"]["batch_size"] / config["main"]["episode_len"]
info["eval_n_alphas"] = alpha_schedule(eval=True)[1]["n_alphas"]
info["eval_n_episodes"] = (
    config["eval"]["episodes_per_alpha_per_env"]
    * info["eval_n_alphas"]
    * config["main"]["n_envs"]
)
info["eval_n_steps"] = info["eval_n_episodes"] * config["main"]["episode_len"]
info["eval_overhead"] = info["eval_n_steps"] / info["rollout_n_steps"]

if __name__ == "__main__":
    print("## Configuration ##")
    print(json.dumps(dict(config=config, info=info), indent=2))
    input("Press Enter to continue.")


###
# W&B init
###

if __name__ == "__main__":
    wandb_tags = config["wandb"]["tags"]
    config.pop("wandb", None)
    print("## WandB init ##")
    wandb.init(
        project="dqn",
        entity="tailstorm",
        tags=wandb_tags,
        config=dict(config=config, info=info),
    )

###
# env
###


def env_fn(eval=False, n_recordings=42):
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
            env, episode_len=config["main"]["episode_len"]
        ),
    )

    env = reward[config["main"]["reward"]](env)

    alpha_f, _ = alpha_schedule(eval=eval)
    env = cpr_gym.wrappers.AlphaScheduleWrapper(env, alpha_schedule=alpha_f)

    if eval:
        env = cpr_gym.wrappers.EpisodeRecorderWrapper(
            env,
            n=n_recordings,
            info_keys=["alpha", "episode_pow_interval"],
        )

    env = cpr_gym.wrappers.ClearInfoWrapper(env)

    return env


if __name__ == "__main__":
    print("## Environment (before vectorization) ##")
    env_fn().render()


def venv_fn(**kwargs):
    def f():
        return env_fn(**kwargs)

    if config["main"]["n_envs"] > 1:
        env = stable_baselines3.common.vec_env.SubprocVecEnv(
            [f] * config["main"]["n_envs"]
        )
    else:
        env = stable_baselines3.common.vec_env.DummyVecEnv([f])

    env = stable_baselines3.common.vec_env.VecMonitor(env)
    return env


###
# Evaluation for each alpha
###


class EvalCallback(stable_baselines3.common.callbacks.EvalCallback):
    def __init__(self, eval_env, **kwargs):
        super().__init__(eval_env, **kwargs)

    def _on_step(self):
        r = super()._on_step()
        if self.eval_freq > 0 and self.n_calls % self.eval_freq == 0:
            # create data frame from buffers in vectorized envs
            buffers = self.eval_env.get_attr("erw_history")
            df = pandas.DataFrame(itertools.chain(*buffers))
            # acc per alpha
            df = df.groupby("alpha").mean()
            # plot metric over alpha
            df2 = df.reset_index()
            table = wandb.Table(
                data=df2,
                columns=list(df2),
            )
            plots = {
                f"plot_over_alpha/{key}": wandb.plot.line(table, "alpha", key)
                for key in list(df)
            }
            # timeline for subset of alpha
            if alpha_schedule(eval=True)[1]["range"]:
                df = df.loc[df.index[0 :: config["eval"]["report_alpha"]]]
            per_alpha = {
                f"eval_per_alpha/{key}/{alpha:.2g}": df.loc[alpha, key]
                for key in list(df)
                for alpha in df.index
            }
            timestep = {"timestep": self.num_timesteps}
            # log
            wandb.log(plots | per_alpha | timestep, commit=True)
            return True
        return r


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

    # we evaluate on a fixed alpha schedule
    eval_env = venv_fn(
        eval=True,
        n_recordings=info["eval_n_alphas"]
        * config["eval"]["episodes_per_alpha_per_env"]
        * config["eval"]["recorder_multiple"],
    )

    model.learn(
        total_timesteps=config["main"]["total_timesteps"],
        callback=[
            wandb.integration.sb3.WandbCallback(
                gradient_save_freq=vec_steps_per_rollout,
                model_save_path=log_dir,
                model_save_freq=vec_steps_per_rollout,
                verbose=0,
            ),
            EvalCallback(
                eval_env,
                best_model_save_path=log_dir,
                log_path=log_dir,
                eval_freq=vec_steps_per_rollout + 1,
                n_eval_episodes=info["eval_n_alphas"]
                * config["main"]["n_envs"]
                * config["eval"]["episodes_per_alpha_per_env"],
                deterministic=True,
                render=False,
            ),
        ],
    )

    model.save(log_dir)
