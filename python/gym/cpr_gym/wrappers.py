import collections
import gym
import itertools
import numpy
import warnings


class SparseRelativeRewardWrapper(gym.Wrapper):
    """
    Overwrites objective function.
    Calculates relative rewards at the end of the episode.
    One reward per episode.
    """

    def reset(self):
        obs = self.env.reset()
        self.srrw_atk = 0
        self.srrw_def = 0
        return obs

    def step(self, action):
        obs, _reward, done, info = self.env.step(action)
        self.srrw_def += info["reward_defender"]
        self.srrw_atk += info["reward_attacker"]
        if done:
            sum = self.srrw_def + self.srrw_atk
            if sum != 0:
                reward = self.srrw_atk / sum
            else:
                reward = 0
        else:
            reward = 0
        return obs, reward, done, info


class SparseRewardPerBlockWrapper(gym.Wrapper):
    """
    Overwrites objective function.
    Calculates reward per confirmed proof-of-work puzzle solution at the end of episode.
    One reward per episode.

    This is similar to the SparseRelativeRewardWrapper. The two wrappers are
    equivalent for protocols or reward functions that assign a constant reward
    per confirmed puzzle solution. For different reward functions, e.g.
    Tailstorm's 'discount' scheme, this wrapper is better suited.
    """

    def reset(self):
        obs = self.env.reset()
        self.srpbw_acc = 0
        self.srpbw_pow = 0
        return obs

    def step(self, action):
        obs, _reward, done, info = self.env.step(action)
        self.srpbw_acc += info["reward_attacker"]
        self.srpbw_pow += info["reward_n_pows"]
        if done:
            if self.srpbw_pow != 0:
                reward = self.srpbw_acc / self.srpbw_pow
            else:
                reward = 0
        else:
            reward = 0
        return obs, reward, done, info


class DenseRewardPerBlockWrapper(gym.Wrapper):
    """
    Mimics SparseRewardPerBlockWrapper but with dense rewards.
    The trick is to end the episode at a given target block height.
    This way, we know the divisor in SparseRewardPerBlockWrapper in advance.
    """

    def __init__(self, env, n_pow=128):
        super().__init__(env)

        self.drpb_stop = n_pow

        for k in ["max_steps", "max_time"]:
            if k in self.env.core_kwargs.keys():
                self.env.core_kwargs.pop(k, None)
                warnings.warn(
                    f"DenseRewardPerBlockWrapper overwrites argument '{k}' in wrapped env"
                )

        self.env.core_kwargs["max_steps"] = n_pow * 10

    def reset(self):
        self.drpb_cnt = 0
        return self.env.reset()

    def step(self, action):
        obs, _reward, done, info = self.env.step(action)

        if info["reward_n_pows"] < 0:
            warnings.warn("negative reward_n_pows")
        if info["reward_attacker"] < 0:
            warnings.warn("negative reward_attacker")

        reward = info["reward_attacker"] / self.drpb_stop
        step = info["reward_n_pows"]

        if step > self.drpb_stop - self.drpb_cnt:
            # we are overshooting self.drpb_stop, resize reward accordingly
            reward = reward * (self.drpb_stop - self.drpb_cnt) / step
            step = self.drpb_stop - self.drpb_cnt

        self.drpb_cnt += step

        if done and self.drpb_cnt < self.drpb_stop:
            warnings.warn("observed less pows than expected")

        if self.drpb_cnt >= self.drpb_stop:
            done = True

        return obs, reward, done, info


class AlphaScheduleWrapper(gym.Wrapper):
    """
    Reconfigures alpha on each reset.
    Extends observation space with current alpha.
    Reports alpha in the info field.
    """

    # TODO. Do we need something similar for gamma? Maybe we can generalize
    # this Wrapper to support arbitrary parameters?

    def __init__(self, env, alpha_schedule=None, normalize_reward=True):
        super().__init__(env)
        self.asw_normalize_reward = normalize_reward
        self.alpha_schedule = alpha_schedule
        if callable(alpha_schedule):
            self.asw_fn = alpha_schedule
        else:
            iterator = itertools.cycle(alpha_schedule)
            self.asw_fn = lambda: next(iterator)

        # extend observation space
        low = self.observation_space.low
        high = self.observation_space.high
        low = numpy.append(low, [0])
        high = numpy.append(high, [1])
        self.observation_space = gym.spaces.Box(low, high, dtype=numpy.float64)

    def observation(self, obs):
        return numpy.append(obs, [self.asw_alpha])

    # overwrite core env's policy
    def policy(self, obs, name="honest"):
        obs = obs[:-1]
        return self.env.policy(obs, name)

    def reset(self):
        self.asw_alpha = self.asw_fn()
        self.env.core_kwargs["alpha"] = self.asw_alpha
        obs = self.env.reset()
        obs = AlphaScheduleWrapper.observation(self, obs)
        return obs

    def step(self, action):
        obs, reward, done, info = self.env.step(action)
        info["alpha"] = self.asw_alpha
        obs = AlphaScheduleWrapper.observation(self, obs)

        if self.asw_normalize_reward:
            reward = reward / self.asw_alpha

        return obs, reward, done, info


class EpisodeRecorderWrapper(gym.Wrapper):
    """
    Records rewards of the last `n` episodes.
    """

    def __init__(self, env, n=42, info_keys=[]):
        super().__init__(env)
        self.erw_info_keys = info_keys
        self.erw_history = collections.deque([], maxlen=n)

    def reset(self):
        self.erw_episode_reward = 0
        return self.env.reset()

    def step(self, action):
        obs, reward, done, info = self.env.step(action)
        self.erw_episode_reward += reward
        if done:
            entry = {k: info[k] for k in self.erw_info_keys}
            entry["episode_reward"] = self.erw_episode_reward
            self.erw_history.append(entry)
        return obs, reward, done, info