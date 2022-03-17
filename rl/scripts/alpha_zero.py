import sys, os

if "." not in sys.path:
    sys.path.append(".")

import ray
from ray import tune
from ray.tune.registry import register_env

from ray.rllib.contrib.alpha_zero.models.custom_torch_models import DenseModel
from ray.rllib.contrib.alpha_zero.environments.cartpole import CartPole
from ray.rllib.models.catalog import ModelCatalog
from ray.rllib.contrib.alpha_zero.core.alpha_zero_trainer import AlphaZeroTrainer

import gym
from cpr_gym import specs
from rl.env_wrapper import EnvWrapper

if __name__ == "__main__":
    ALPHA = 0.3
    spec = specs.nakamoto(alpha=ALPHA)
    register_env("nakamoto", lambda _: EnvWrapper(spec))
    ray.init()

    ModelCatalog.register_custom_model("dense_model", DenseModel)

    config = {
        "num_workers": 1,
        "rollout_fragment_length": 50,
        "train_batch_size": 500,
        "sgd_minibatch_size": 64,
        "lr": 1e-4,
        "num_sgd_iter": 1,
        "mcts_config": {
            "puct_coefficient": 1.5,
            "num_simulations": 100,
            "temperature": 1.0,
            "dirichlet_epsilon": 0.20,
            "dirichlet_noise": 0.03,
            "argmax_tree_policy": False,
            "add_dirichlet_noise": True,
        },
        "ranked_rewards": {
            "enable": True,
        },
        "model": {
            "custom_model": "dense_model",
        },
    }

    agent = AlphaZeroTrainer(config=config, env="nakamoto")
    for _ in range(100):
        agent.train()
