import re
import gym
import numpy as np
from gym.spaces import Tuple, MultiDiscrete
from typing import List
from rl.utils import Config


class AlphaScheduleWrapper(gym.Wrapper):
    def __init__(self, env, env_fn, config: Config):
        super().__init__(env)
        self.alpha_schedule = config.ALPHA_SCHEDULE
        self.env_fn = env_fn
        self.current_step = 0
        self.alpha = None
        self.config = config
        self.in_prep_phase = False
        low = self.observation_space.low
        high = self.observation_space.high
        low = np.append(low, [0])
        high = np.append(high, [1])
        self.observation_space = gym.spaces.Box(low, high, dtype=np.float64)

    def update_difficulties(self, reset_difficulties=False):
        if reset_difficulties:
            self.difficulties = dict((a, self.target) for a in self.alpha_schedule)
        else:
            if self.alpha:
                difficulty = self.difficulties[self.alpha] * (
                    self.target / self.observed
                )
                self.difficulties[self.alpha] = difficulty
                self.env = self.env_fn(
                    alpha=self.alpha,
                    target=self.difficulties.get(self.alpha, self.target),
                    config=self.config,
                )
                obs = self.env.reset()
                return obs

    def reset(self, reset_difficulties=False):
        self.current_step = 0
        alpha = np.random.choice(self.alpha_schedule)

        self.env = self.env_fn(
            alpha=alpha, target=self.config.ACTIVATION_DELAY, config=self.config
        )
        self.alpha = alpha

        obs = self.observation(self.env.reset())

        return obs

    def policy(self, obs, name="honest"):
        return self.env.policy(obs[:-1], name)

    def observation(self, obs):
        obs = np.append(obs, self.alpha)
        return obs

    def step(self, action):
        obs, reward, done, info = self.env.step(action)
        self.info = info
        self.current_step += 1
        obs = self.observation(obs)

        # if done and self.run_daa:
        #     self.observed = info['simulator_clock_rewarded'] / self.n_pow
        return obs, reward, done, info
