import re
import gym
import numpy as np
from gym.spaces import Tuple, MultiDiscrete


class HonestPolicyWrapper(gym.Wrapper):
    def __init__(self, env, n_steps):
        super().__init__(env)
        self.current_state = None
        self.current_step = 0
        self.n_steps = n_steps

    def reset(self):
        self.current_step += 1
        obs = self.env.reset()
        self.current_state = obs
        return obs

    def step(self, action):
        if self.current_step < self.n_steps:
            if self.current_state[1] > self.current_state[0]:
                action = 1
            elif self.current_state[1] < self.current_state[0]:
                action = 0
            else:
                action = 3
        next_state, reward, done, info = self.env.step(action)
        self.current_state = next_state
        return next_state, reward, done, info
