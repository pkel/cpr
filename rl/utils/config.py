from pydantic import BaseSettings
from typing import List, Literal


class Config(BaseSettings):
    PROTOCOL: Literal["tailstorm", "nakamoto", "bk", "bk_ll"] = "tailstorm"
    REWARD_SCHEME: Literal["discount", "constant"] = "constant"
    K: int = 8
    ALGO: Literal["PPO", "DQN"] = "PPO"
    TOTAL_TIMESTEPS: int = 10e9
    STEPS_PER_ROLLOUT: int = 200
    STARTING_LR: float = 10e-3
    ENDING_LR: float = 10e-5
    BATCH_SIZE: int = 200_000
    ALPHA_SCHEDULE_CUTOFF: float = 0
    LAYER_SIZE: int = 256
    N_LAYERS: int = 2
    N_STEPS_MULTIPLE: int = 1
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
    GAMMA: float = 0.5
    DEFENDERS: int = 2
    ACTIVATION_DELAY: int = 1
    N_ENVS: int = 16


config = Config()