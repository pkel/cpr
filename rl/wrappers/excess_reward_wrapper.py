import collections
import gym
import numpy as np
from gym.spaces import Tuple, MultiDiscrete


class SparseDaaRewardWrapper(gym.Wrapper):
    def ___init__(self, env):
        super().__init__(env)
        self.n_pow = 0
        self.sum_attacker = 0

    def reset(self):
        self.n_pow = 0
        self.sum_attacker = 0
        return self.env.reset()

    def step(self, action):
        obs, reward, done, info = self.env.step(action)
        self.n_pow += info["reward_n_pows"]
        self.sum_attacker += info["reward_attacker"]
        if done:
            observed = info["simulator_clock_now"] / self.n_pow
            reward = self.sum_attacker * observed
        return obs, reward, done, info


class AbsoluteRewardWrapper(gym.Wrapper):
    def __init__(self, env, normalize=True):
        super().__init__(env)
        self.rolling_reward = dict()
        self.current_ep_reward = 0
        self.normalize = normalize

    def step(self, action):
        obs, reward, done, info = self.env.step(action)
        reward = info["reward_attacker"]

        if done:
            if self.env.alpha not in self.rolling_reward:
                # take last 5000 rewards
                self.rolling_reward[self.env.alpha] = collections.deque([], maxlen=5000)
            self.rolling_reward[self.env.alpha].append(self.current_ep_reward)
            self.current_ep_reward = 0
        if self.normalize:
            reward /= self.env.config["STEPS_PER_ROLLOUT"]
        if info.get("in_prep_phase", False):
            reward = 0
        self.current_ep_reward += reward
        return obs, reward, done, info


class WastedBlocksRewardWrapper(gym.Wrapper):
    def __init__(self, env):
        super().__init__(env)
        self.current_obs = None
        self.rolling_reward = dict()
        self.current_ep_reward = 0
        # r = alpha, penalty = 1 - alpha

    def reset(self):
        obs = self.env.reset()
        self.current_obs = obs
        return obs

    def step(self, action):
        next_state, reward, done, info = self.env.step(action)
        # Attacker succeeds in overriding
        if (
            info["reward_attacker"] > 0
        ):  # and info['reward_attacker'] == self.current_obs[1]:
            reward = self.env.alpha * self.current_obs[0]
        elif (
            info["reward_defender"] > 0
        ):  # and info['reward_defender'] == self.current_obs[0]:
            reward = -1 * (1 - self.env.alpha) * self.current_obs[1]
        else:
            reward = 0
        self.current_obs = next_state
        self.current_ep_reward += reward
        if done:
            self.rolling_reward[self.env.alpha].append(self.current_ep_reward)
            # take last 5000 rewards
            if self.env.alpha not in self.rolling_reward:
                # take last 5000 rewards
                self.rolling_reward[self.env.alpha] = collections.deque([], maxlen=5000)
            self.rolling_reward[self.env.alpha].append(self.current_ep_reward)
            self.current_ep_reward = 0
        return next_state, reward, done, info


class SparseRelativeRewardWrapper(gym.Wrapper):
    def __init__(self, env, relative=True):
        super().__init__(env)
        self.relative = relative
        self.sum_attacker = 0
        self.sum_defender = 0
        self.current_relative_reward = 0
        self.last_relative_reward = 0

    def reset(self, *args, **kwargs):
        obs = self.env.reset(*args, **kwargs)
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
