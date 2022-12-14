from pydantic import BaseSettings
from typing import List, Literal


class Config(BaseSettings):
    PROTOCOL: Literal[
        "tailstorm", "nakamoto", "bk", "bk_ll", "tailstormjune"
    ] = "tailstormjune"
    REWARD_SCHEME: Literal["discount", "constant"] = "discount"
    K: int = 8
    ALGO: Literal["PPO", "DQN"] = "PPO"
    TOTAL_TIMESTEPS: int = 1e9
    STEPS_PER_ROLLOUT: int = 200
    STARTING_LR: float = 1e-4
    ENDING_LR: float = 1e-6
    BATCH_SIZE: int = 2048
    ALPHA_SCHEDULE_CUTOFF: float = 0
    LAYER_SIZE: int = 100
    N_LAYERS: int = 2
    N_STEPS_MULTIPLE: int = 100
    HONEST_STEPS_FRACTION: float = 0.1
    STARTING_EPS: float = 0.99
    ENDING_EPS: float = 0.01
    ALPHA_SCHEDULE: List[float] = [
        0.15,
        0.25,
        1 / 3.0,
        0.35,
        0.375,
        0.4,
        0.425,
        0.45,
        0.475,
    ]
    USE_DAA: bool = True
    DAA_METHOD: Literal["sparse", "dense"] = "sparse"
    GAMMA: float = 0.9
    DEFENDERS: int = 50
    ACTIVATION_DELAY: int = 1
    N_ENVS: int = 16


config = Config()
