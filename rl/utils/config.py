from pydantic import BaseSettings
from typing import List, Literal


class Config(BaseSettings):
    PROTOCOL: Literal[
        "tailstorm", "nakamoto", "bk", "bk_ll", "tailstormll"
    ] = "tailstormll"
    REWARD_SCHEME: Literal["discount", "constant"] = "discount"
    K: int = 8
    ALGO: Literal["PPO", "DQN"] = "PPO"
    TOTAL_TIMESTEPS: int = 50e7
    STEPS_PER_ROLLOUT: int = 200
    STARTING_LR: float = 0.0001
    ENDING_LR: float = 0.000001
    BATCH_SIZE: int = 2048
    ALPHA_SCHEDULE_CUTOFF: float = 0
    LAYER_SIZE: int = 100
    N_LAYERS: int = 2
    N_STEPS_MULTIPLE: int = 10
    STARTING_EPS: float = 0.99
    ENDING_EPS: float = 0.01
    ALPHA_SCHEDULE: List[float] = [
        0.1,
        # 0.15,
        # 0.175,
        # 0.2,
        0.25,
        1 / 3.0,
        0.35,
        0.375,
        0.4,
        0.425,
        0.45,
        0.475,
    ]
    REWARD_WRAPPER: Literal[
        "SparseDaaRewardWrapper",
        "WastedBlocksRewardWrapper",
        "BlocksPerProgressRewardWrapper",
    ] = "SparseDaaRewardWrapper"
    GAMMA: float = 0.5
    DEFENDERS: int = 2
    ACTIVATION_DELAY: int = 1
    N_ENVS: int = 16
    SUBBLOCK_SELECTION: Literal["heuristic", "optimal"] = "optimal"
    USE_SDE: bool = False
    SPARSE_REWARD: bool = True
    # ENTROPY_COEFFICIENT: float = 0.01


config = Config()
