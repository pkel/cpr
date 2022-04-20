import re
import gym
import numpy as np
from gym.spaces import Tuple, MultiDiscrete


class IllegalMoveWrapper(gym.Wrapper):
    def __init__(self, env):
        super().__init__(env)
        self.current_state = None

    def reset(self):
        obs = self.env.reset()
        self.current_state = obs
        return obs

    def step(self, action):
        next_state, reward, done, info = self.env.step(action)
        if self.current_state[2] <= 0 and action == 1:
            reward = -2
            done = True
        self.current_state = next_state
        return next_state, reward, done, info
