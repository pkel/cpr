import re
import gym
import numpy as np
from gym.spaces import Tuple, MultiDiscrete


class SparseRelativeRewardWrapper(gym.Wrapper):
    def __init__(self, env):
        super().__init__(env)
        self.sum_attacker = 0
        self.sum_defender = 0
        self.current_relative_reward = 0
        self.last_relative_reward = 0

    def reset(self):
        obs = self.env.reset()
        self.sum_attacker = 0
        self.sum_defender = 0
        self.current_relative_reward = 0
        self.last_relative_reward = 0
        return obs

    def step(self, action):
        next_state, reward, done, info = self.env.step(action)
        self.sum_attacker += info["reward_attacker"]
        self.sum_defender += info["reward_defender"]
        if done:
            reward = (((self.sum_attacker) / (self.sum_defender + self.sum_attacker + 1e-8)) - self.env.alpha) / self.env.alpha
        else:
            reward = 0
        return next_state, reward, done, info

class RelativeRewardWrapper(gym.Wrapper):
    def __init__(self, env, alpha, max_steps=1000):
        super().__init__(env)
        self.alpha = alpha
        self.max_steps = max_steps
        self.sum_attacker = 0
        self.sum_defender = 0
        self.current_relative_reward = 0
        self.last_relative_reward = 0

    def reset(self):
        obs = self.env.reset()
        self.sum_attacker = 0
        self.sum_defender = 0
        self.current_relative_reward = 0
        self.last_relative_reward = 0
        return obs

    def step(self, action):
        next_state, reward, done, info = self.env.step(action)
        self.sum_attacker += info["reward_attacker"]
        self.sum_defender += info["reward_defender"]
        self.last_relative_reward = self.current_relative_reward
        self.current_relative_reward = self.sum_attacker / (
            self.sum_defender + self.sum_attacker + 1e-8
        )
        reward = self.current_relative_reward - self.last_relative_reward
        return next_state, reward, done, info
    