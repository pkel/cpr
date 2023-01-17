import argparse
import cfg_model
import cpr_gym
import cpr_gym.wrappers
import gym
import itertools
import json
import numpy
import os
import pandas
import random
import socket
import stable_baselines3
import torch
import utils
import wandb
import wandb.integration.sb3

###
# load config
###

parser = argparse.ArgumentParser(description="Train PPO against cpr_gym.")
parser.add_argument(
    "protocol",
    metavar="PROTOCOL",
    help="a protocol with PROTOCOL.yaml file under configs/",
)
parser.add_argument(
    "--alpha",
    metavar="INT",
    type=int,
    help="compute capability of the attacker (percent)",
    required=True,
)
parser.add_argument(
    "--gamma",
    metavar="INT",
    type=int,
    help="network capability of the attacker (percent)",
    required=True,
)
parser.add_argument(
    "--shape",
    metavar="STR",
    type=str,
    help="apply reward shaping (raw, cut, exp) (default: raw)",
    required=False,
    default="raw",
)
parser.add_argument(
    "--batch",
    action=argparse.BooleanOptionalAction,
    help="skip interaction before training",
)
parser.add_argument(
    "--tag",
    action="append",
    default=[],
    help="apply WandB tag",
)
args = parser.parse_args()

loc = os.path.dirname(__file__)

with open(os.path.join(loc, "configs", args.protocol + ".yaml"), "r") as f:
    config = cfg_model.Config.parse_raw(f.read())

config.env.gamma = args.gamma / 100
config.main.alpha = args.alpha / 100
config.env.shape = args.shape
config.wandb.tags += args.tag

task = f"{args.protocol}-alpha{args.alpha:02d}-gamma{args.gamma:02d}-{args.shape}"

os.chdir(loc)

torch.set_num_threads(config.main.torch_threads)

###
# Alpha Schedule
###


def alpha_schedule(eval=False):
    info = dict()
    if isinstance(config.main.alpha, list):
        alphas = config.main.alpha

        if eval:
            info["n_alphas"] = len(alphas)
            info["range"] = False
            alpha_schedule = alphas

        def alpha_schedule():
            return random.choice(alphas)

    elif isinstance(config.main.alpha, cfg_model.Range):
        if eval:
            alpha_schedule = numpy.arange(
                config.main.alpha.min,
                numpy.nextafter(config.main.alpha.max, 1),
                config.eval.alpha_step,
            )
            info["n_alphas"] = numpy.size(alpha_schedule)
            info["range"] = True
        else:

            def alpha_schedule():
                return random.uniform(config.main.alpha.min, config.main.alpha.max)

    else:
        if eval:
            info["n_alphas"] = 1
            info["range"] = False

        alpha_schedule = [config.main.alpha]

    return alpha_schedule, info


info = dict()
info["task"] = task
info["host"] = socket.gethostname()
info["version"] = cpr_gym.__version__
info["episode_n_steps"] = config.env.episode_len
info["rollout_n_steps"] = (
    config.ppo.batch_size * config.ppo.n_steps_multiple * config.main.n_envs
)
info["rollout_n_episodes"] = info["rollout_n_steps"] / config.env.episode_len
info["rollout_n_batches"] = info["rollout_n_steps"] / config.ppo.batch_size
info["batch_n_steps"] = config.ppo.batch_size
info["batch_n_episodes"] = config.ppo.batch_size / config.env.episode_len
info["eval_n_alphas"] = alpha_schedule(eval=True)[1]["n_alphas"]
info["eval_n_episodes"] = (
    config.eval.episodes_per_alpha_per_env * info["eval_n_alphas"] * config.main.n_envs
)
info["eval_n_steps"] = info["eval_n_episodes"] * config.env.episode_len
info["eval_overhead"] = (
    info["eval_n_steps"] / info["rollout_n_steps"] / config.eval.freq
)

dirty = "dirty" in cpr_gym.__version__

if __name__ == "__main__":
    print("## Configuration ##")
    print(json.dumps(dict(config=config.dict(), info=info), indent=2))
    if dirty:
        print("OFFLINE: will set WANDB_MODE=offline due to dirty version")
    if not args.batch:
        input("Press Enter to continue.")


###
# W&B init
###

if __name__ == "__main__":
    wandb_kwargs = dict(tags=config.wandb.tags)
    cfg = config.dict()
    cfg.pop("wandb", None)
    print("## WandB init ##")
    if dirty:
        wandb_kwargs["mode"] = "offline"
    wandb.init(
        project="cpr-v0.7-ppo",
        entity="tailstorm",
        config=dict(config=cfg, info=info),
        **wandb_kwargs,
    )
    wandb.run.name = f"{task}-{wandb.run.id}"

###
# env
###


def env_fn(eval=False, n_recordings=42):
    env_args = config.env.dict()
    protocol_args = config.protocol.dict()
    env_args["protocol"] = protocol_args.pop("name")
    env_args["protocol_args"] = protocol_args

    alpha_f, _ = alpha_schedule(eval=eval)
    env_args["alpha"] = alpha_f

    env_args["normalize_reward"] = False  # (x / alpha) mapping is done below

    shape = env_args.pop("shape")
    if not env_args["reward"].startswith("sparse_") and not shape == "raw":
        raise ValueError("reward shaping requires sparse reward scheme")

    env = gym.make(env_args.pop("name"), **env_args)

    # reward shaping
    if eval:
        env = cpr_gym.wrappers.MapRewardWrapper(env, lambda r, i: r / i["alpha"])
    else:
        if shape == "cut":

            # set reward = 0 if behaviour seems honest
            def cut(r, i):
                if r <= 0.0 or i["episode_progress"] <= 0.0:
                    return 0.0
                orphans = i["episode_n_activations"] / i["episode_progress"]
                if orphans <= 1.05:
                    return 0.0
                else:
                    return r / i["alpha"]

            env = cpr_gym.wrappers.MapRewardWrapper(env, cut)
        elif shape == "exp":

            def exp(r, i):
                if r <= 0.0:
                    return 0.0
                return numpy.exp(r - 1.0) / i["alpha"]

            env = cpr_gym.wrappers.MapRewardWrapper(env, exp)
        elif shape == "raw":
            env = cpr_gym.wrappers.MapRewardWrapper(env, lambda r, i: r / i["alpha"])
        else:
            raise ValueError("unknown reward shape")

    # data recording
    if eval:
        env = cpr_gym.wrappers.EpisodeRecorderWrapper(
            env,
            n=n_recordings,
            info_keys=[
                "alpha",
                "gamma",
                "episode_sim_time",
                "episode_chain_time",
                "episode_progress",
                "episode_n_activations",
            ],
        )

    # Uncomment to let agent observe the progress within the episode
    # fields = []
    # fields.append(((lambda self, info: info["episode_progress"]), 0, float("inf"), 0))
    # fields.append(((lambda self, info: info["episode_chain_time"]), 0, float("inf"), 0))
    # fields.append(((lambda self, info: info["episode_n_steps"]), 0, float("inf"), 0))
    # env = cpr_gym.wrappers.ExtendObservationWrapper(env, fields)

    env = cpr_gym.wrappers.ClearInfoWrapper(env)

    return env


if __name__ == "__main__":
    print("## Environment (before vectorization) ##")
    env_fn().render()


def venv_fn(**kwargs):
    def f():
        return env_fn(**kwargs)

    if config.main.n_envs > 1:
        env = stable_baselines3.common.vec_env.SubprocVecEnv([f] * config.main.n_envs)
    else:
        env = stable_baselines3.common.vec_env.DummyVecEnv([f])

    env = stable_baselines3.common.vec_env.VecMonitor(env)
    return env


###
# Evaluation for each alpha
###


class EvalCallback(stable_baselines3.common.callbacks.EvalCallback):
    def __init__(self, eval_env, start_at_iteration=0, eval_freq=1, **kwargs):
        super().__init__(eval_env, eval_freq=1, **kwargs)
        self.iteration = -1
        self.first_iteration = start_at_iteration
        self.iteration_eval_freq = eval_freq

    # Parent counts steps and acts every eval_freq steps.
    # We overwrite this to act on the beginning of each iteration.
    def _on_step(self):
        return True

    def _on_rollout_start(self):
        self.iteration += 1

        # dump logger, it has buffered content from the last training phase
        if self.iteration > 0:
            self.logger.record("time/iterations", self.iteration, exclude="tensorboard")
            self.logger.record(
                "time/total_timesteps", self.num_timesteps, exclude="tensorboard"
            )
            self.logger.dump(self.num_timesteps)

        # skip first evaluations; deterministic policy might cause massive slowdown
        if self.iteration < self.first_iteration:
            return True

        # do only one evaluation every eval_freq iterations
        if self.iteration % self.iteration_eval_freq != 0:
            return True

        # pre-load logger with iteration
        self.logger.record("time/iterations", self.iteration, exclude="tensorboard")

        # do parent evaluation as usual
        r = super()._on_step()

        # add own stuff of top

        # create data frame from buffers in vectorized envs
        buffers = self.eval_env.get_attr("erw_history")
        df = pandas.DataFrame(itertools.chain(*buffers)).drop(
            columns=["alpha", "gamma"]
        )
        mean = {"eval/mean_" + k: v for k, v in df.mean().to_dict().items()}
        std = {"eval/std_" + k: v for k, v in df.std().to_dict().items()}

        # # acc per alpha
        # # might become relevant again, if we decide to train range of alphas
        # df = df.groupby("alpha").mean()
        # # plot metric over alpha
        # df2 = df.reset_index()
        # table = wandb.Table(
        #     data=df2,
        #     columns=list(df2),
        # )
        # plots = {
        #     f"plot_over_alpha/{key}": wandb.plot.line(table, "alpha", key)
        #     for key in list(df)
        # }

        # # timeline for subset of alpha
        # if alpha_schedule(eval=True)[1]["range"]:
        #     df = df.loc[df.index[0 :: config["eval"]["report_alpha"]]]
        # per_alpha = {
        #     f"eval_per_alpha/{key}/{alpha:.2g}": df.loc[alpha, key]
        #     for key in list(df)
        #     for alpha in df.index
        # }

        time = {
            "time/total_timesteps": self.num_timesteps,
            "time/iterations": self.iteration,
        }

        # log
        wandb.log(mean | std | time, commit=True)

        return r


###
# Training
###

if __name__ == "__main__":
    print("## Training ##")

    def lr_schedule(remaining):
        return config.ppo.starting_lr * remaining + config.ppo.ending_lr * (
            1 - remaining
        )

    log_dir = f"saved_models/ppo-{task}-{wandb.run.id}"
    print("Use output directory " + log_dir)

    vec_steps_per_rollout = config.ppo.batch_size * config.ppo.n_steps_multiple
    # rollout buffer is this time n_envs

    model = stable_baselines3.PPO(
        "MlpPolicy",
        env=venv_fn(),
        verbose=1,
        batch_size=config.ppo.batch_size,
        gamma=config.ppo.gamma,
        n_steps=vec_steps_per_rollout,
        clip_range=0.1,
        # ent_coef=0.01,
        learning_rate=lr_schedule,
        # clip_range=clip_schedule,
        policy_kwargs=dict(
            activation_fn=torch.nn.ReLU,
            net_arch=[
                dict(
                    pi=[config.ppo.layer_size] * config.ppo.n_layers,
                    vf=[config.ppo.layer_size] * config.ppo.n_layers,
                )
            ],
        ),
    )

    utils.setWandbLogger(model)

    # we evaluate on a fixed alpha schedule
    eval_env = venv_fn(
        eval=True,
        n_recordings=info["eval_n_alphas"]
        * config.eval.episodes_per_alpha_per_env
        * config.eval.recorder_multiple,
    )

    model.learn(
        total_timesteps=config.main.total_timesteps,
        callback=[
            wandb.integration.sb3.WandbCallback(
                gradient_save_freq=vec_steps_per_rollout,
                model_save_path=log_dir,
                model_save_freq=vec_steps_per_rollout,
                verbose=0,
            ),
            EvalCallback(
                eval_env,
                start_at_iteration=config.eval.start_at_iteration,
                best_model_save_path=log_dir,
                log_path=log_dir,
                eval_freq=config.eval.freq,
                n_eval_episodes=info["eval_n_alphas"]
                * config.main.n_envs
                * config.eval.episodes_per_alpha_per_env,
                deterministic=True,
                render=False,
            ),
        ],
    )

    model.save(log_dir)
