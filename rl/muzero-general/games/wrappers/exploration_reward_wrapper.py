import re
import gym
import numpy as np
from gym.spaces import Tuple, MultiDiscrete


class ExplorationRewardWrapper(gym.Wrapper):
    def __init__(self, env, alpha, max_steps=1000, scaling=0.5):
        super().__init__(env)
        self.alpha = alpha
        self.max_steps = max_steps
        self.scaling = scaling
        self.current_step = 0

    def reset(self):
        self.current_step += 1
        return self.env.reset()

    def step(self, action):
        next_state, reward, done, info = self.env.step(action)
        exploration_reward = (
            int(action != 0) * (self.alpha / self.max_steps) * self.scaling
        )
        reward += exploration_reward
        return next_state, reward, done, info
