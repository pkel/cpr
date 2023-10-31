import psutil
from pydantic import BaseModel
from pydantic_yaml import YamlModel
from typing import Literal, Union  # revise on Python 3.10


###
# Protocols
###


class Nakamoto(BaseModel):
    name: Literal["nakamoto"] = "nakamoto"


class Parallel(BaseModel):
    name: Literal["bk", "spar"]
    k: int
    reward: Literal["constant", "block"] = "constant"


class Tree(BaseModel):
    name: Literal["stree", "tailstorm"]
    k: int
    reward: Literal["constant", "discount", "hybrid", "punish"] = "constant"
    subblock_selection: Literal["altruistic", "heuristic", "optimal"] = "heuristic"


class TailstormJune(BaseModel):
    name: Literal["tailstormjune"]
    k: int
    reward: Literal["block", "constant", "discount", "hybrid", "punish"] = "constant"


Protocol = Union[Nakamoto, Parallel, Tree, TailstormJune]


###
# Training
###


class Range(BaseModel):
    min: float
    max: float


class Main(BaseModel):
    n_envs: int = psutil.cpu_count()
    torch_threads: int = psutil.cpu_count()
    alpha: Union[Range, list[float], float]
    total_timesteps: int


class Env(BaseModel):
    name = "cpr_gym:cpr-v0"
    activation_delay = 1.0
    gamma = 0.5
    defenders = 100
    episode_len = 128
    reward: Literal[
        "sparse_relative",
        "sparse_per_progress",
        "dense_per_progress",
    ] = "sparse_relative"
    shape: Literal[
        "raw",  # use reward function as is
        "cut",  # punish honest behaviour with reward=0.9
        "exp",  # scale reward exponentially
    ] = "raw"


class Eval(BaseModel):
    # Run evaluation once per freq iterations
    freq = 1

    # We evaluate on deterministic policies. Early during training, the policy
    # might be very dumb. It seems to cause massive simulation overhead. Thus we
    # skip evaluation in the beginning.
    start_at_iteration = 1

    # If we train on single alpha or list of alphas we evaluate on all alphas.
    # If we train on a range of alphas we use alpha_step to derive a list of
    # alphas to evaluate on.
    alpha_step: float = 0.025

    # How many episodes per evaluation per n_env per alpha?
    episodes_per_alpha_per_env: int

    # For reporting, we maintain a ring buffer of past episodes in each n_env.
    # When reporting per_alpha statistics, we calculate the mean over this ring
    # buffer. The number of recorded episodes is
    # episodes_per_alpha_per_env * # alphas * n_envs * recorder_multiple
    recorder_multiple = 1

    # Reporting per_alpha statistics for all alphas might overload the dashboard.
    # Set report_alpha = n to report only every n-th alpha.
    report_alpha = 1


class LinearSchedule(BaseModel):
    schedule: Literal["linear"] = "linear"
    start: float
    end: float


Schedule = Union[float, LinearSchedule]


class PPO(BaseModel):
    batch_size: int
    gamma: float
    n_steps_multiple: int
    n_layers: int
    layer_size: int
    ent_coef: float = 0.0
    learning_rate: Schedule


class WandB(BaseModel):
    tags: list[str] = []


class Config(YamlModel):
    main: Main
    env = Env()
    protocol: Protocol = Nakamoto()
    eval: Eval
    ppo: PPO
    wandb = WandB()
