import re
import gym
import numpy as np
from gym.spaces import Tuple, MultiDiscrete
from typing import List


class AlphaScheduleWrapper(gym.Wrapper):
    def __init__(self, env, env_fn, config):
        super().__init__(env)
        self.alpha_schedule = config["ALPHA_SCHEDULE"]
        self.env_fn = env_fn
        self.current_step = 0
        self.alpha = None
        self.config = config

        # DAA
        self.run_daa = config["USE_DAA"]
        if self.run_daa:
            self.target = config["ACTIVATION_DELAY"]
            self.difficulties = dict((a, self.target) for a in config["ALPHA_SCHEDULE"])
            self.n_pow = 0
            self.observed = self.target
            self.in_prep_phase = True

        self.observation_space = gym.spaces.Box(-np.inf, np.inf, (5,), dtype=np.float64)

    def update_difficulties(self, reset_difficulties):
        if reset_difficulties:
            self.difficulties = dict((a, self.target) for a in self.alpha_schedule)
        else:
            if self.alpha:
                difficulty = self.difficulties[self.alpha] * (
                    self.target / self.observed
                )
                self.difficulties[self.alpha] = difficulty
                self.n_pow = 0
                self.observed = 0

    def reset(self, reset_difficulties=False):
        self.current_step = 0
        alpha = np.random.choice(self.alpha_schedule)

        if self.run_daa:
            self.update_difficulties(reset_difficulties)
            self.in_prep_phase = True
            self.env = self.env_fn(
                alpha=alpha, target=self.difficulties[alpha], config=self.config
            )
        else:
            self.env = self.env_fn(
                alpha=alpha, target=self.config["ACTIVATION_DELAY"], config=self.config
            )
        self.alpha = alpha

        obs = self.observation(self.env.reset())

        return obs

    def observation(self, obs):
        obs = np.append(obs, int(self.in_prep_phase))
        return obs

    def step(self, action):
        obs, reward, done, info = self.env.step(action)

        self.current_step += 1
        if self.run_daa:
            self.n_pow += info["reward_n_pows"]
            if self.n_pow > 0:
                self.observed = info["simulator_clock_rewarded"] / self.n_pow

            if self.current_step == self.config["STEPS_PER_ROLLOUT"] // 2:
                self.update_difficulties()
                self.in_prep_phase = False

            info["difficulties"] = self.difficulties
            info["in_prep_phase"] = self.in_prep_phase
            obs = self.observation(obs)

        # if done and self.run_daa:
        #     self.observed = info['simulator_clock_rewarded'] / self.n_pow
        return obs, reward, done, info
