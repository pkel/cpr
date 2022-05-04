import re
import gym
import numpy as np
from gym.spaces import Tuple, MultiDiscrete


class AbsoluteRewardWrapper(gym.Wrapper):
    def __init__(self, env):
        super().__init__(env)
    
    def step(self, action):
        obs, reward, done, info = self.env.step(action)
        reward = info["reward_attacker"]
        return obs, reward, done, info
        
class WastedBlocksRewardWrapper(gym.Wrapper):
    def __init__(self, env):
        super().__init__(env)
        self.current_obs = None
        
        # r = alpha, penalty = 1 - alpha

    def reset(self):
        obs = self.env.reset()
        self.current_obs = obs
        return obs

    def step(self, action):
        next_state, reward, done, info = self.env.step(action)
        # Attacker succeeds in overriding
        if info['reward_attacker'] > 0: # and info['reward_attacker'] == self.current_obs[1]:
            reward = self.env.alpha * self.current_obs[0]
        elif info['reward_defender'] > 0: # and info['reward_defender'] == self.current_obs[0]:
            reward = -1 * (1 - self.env.alpha)* self.current_obs[1]
        else:
            reward = 0
        self.current_obs = next_state
        return next_state, reward, done, info

class SparseRelativeRewardWrapper(gym.Wrapper):
    def __init__(self, env, relative=True):
        super().__init__(env)
        self.relative = relative
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
            try:
                reward = self.sum_attacker / (self.sum_defender + self.sum_attacker)
            except:
                reward = 0
            if self.relative:
                reward -= self.env.alpha
                reward /= self.env.alpha
                # reward = 1 if reward > self.env.alpha else 0
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
    