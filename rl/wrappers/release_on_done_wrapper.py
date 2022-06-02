import collections
import gym
import numpy as np
from gym.spaces import Tuple, MultiDiscrete


class ReleaseOnDoneWrapper(gym.Wrapper):
    def __init__(self, env) -> None:
        super().__init__(env)

    def step(self, action):
        obs, reward, done, info = self.env.step(action)
        if done:
            public_blocks = obs[0]
            private_blocks = obs[1]
            info["reward_n_pows"] = max(public_blocks, private_blocks)
            if public_blocks > private_blocks:
                info["reward_defender"] = public_blocks
            elif private_blocks > public_blocks:
                info["reward_attacker"] = private_blocks
            elif public_blocks == private_blocks:
                private_wins = np.random.choice(
                    [True, False], p=[self.env.alpha, 1 - self.env.alpha]
                )
                if private_wins:
                    info["reward_attacker"] = private_blocks
                else:
                    info["reward_defender"] = public_blocks

        return obs, reward, done, info
