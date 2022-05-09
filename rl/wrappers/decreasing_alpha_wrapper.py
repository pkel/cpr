import re
import gym
import numpy as np
from gym.spaces import Tuple, MultiDiscrete
from typing import List


class AlphaScheduleWrapper(gym.Wrapper):
    def __init__(self, env, env_fn, alpha_schedule: List, run_daa=False, target=600):
        super().__init__(env)
        self.alpha_schedule = alpha_schedule
        self.env_fn = env_fn
        self.current_step = 0
        self.alpha = None

        # DAA
        self.run_daa = run_daa
        if self.run_daa:
            self.target = target
            self.difficulties = dict((a, self.target) for a in alpha_schedule)
            self.n_pow = 0
            self.observed = self.target

    
    def update_difficulties(self):
        difficulty = self.target * self.target / self.observed
        self.difficulties[self.alpha] = difficulty
        self.n_pow = 0
        self.observed = 0
        

    def reset(self):
        self.current_step += 1
        alpha = np.random.choice(self.alpha_schedule)

        if self.run_daa:
            self.update_difficulties()

            self.env = self.env_fn(alpha=alpha, target=self.difficulties[alpha])
        else:
            self.env = self.env_fn(alpha=alpha, target=None)
        self.alpha = alpha
        
        obs = self.env.reset()
        
        return obs

    def step(self, action):
        obs, reward, done, info = self.env.step(action)
        if self.run_daa:
            self.n_pow += info["reward_n_pows"]
        if done and self.run_daa:
            self.observed = info['simulator_clock_rewarded'] / self.n_pow
        return obs, reward, done, info

    