import re
import gym
import numpy as np
from gym.spaces import Tuple, MultiDiscrete


class AlphaScheduleWrapper(gym.Wrapper):
    def __init__(self, env, env_fn, alpha_schedule):
        super().__init__(env)
        self.alpha_schedule = alpha_schedule
        self.env_fn = env_fn
        self.current_step = 0
        self.alpha = None
        

    def reset(self):
        self.current_step += 1
        alpha = self.alpha_schedule(self.current_step)
        self.env = self.env_fn(alpha=alpha)
        self.alpha = alpha
        obs = self.env.reset()
        
        return obs

    