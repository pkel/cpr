import collections
import gym
import itertools
import numpy
import warnings


class SparseRelativeRewardWrapper(gym.Wrapper):
    """
    Overwrites objective function.
    Calculates relative rewards at the end of the episode.
    """

    def step(self, action):
        obs, _reward, done, info = self.env.step(action)
        if done:
            attacker = info["episode_reward_attacker"]
            defender = info["episode_reward_defender"]
            sum = attacker + defender
            if sum != 0:
                reward = attacker / sum
            else:
                reward = 0
        else:
            reward = 0
        return obs, reward, done, info


class SparseRewardPerProgressWrapper(gym.Wrapper):
    """
    Overwrites objective function.
    Calculates reward per chain progress at the end of episode.

    Equivalent to SparseRelativeRewardWrapper for Nakamoto but not for
    protocols with dynamic rewards or progress.
    SparseRewardPerProgressWrapper is better suited for protocols like Ethereum
    or Tailstorm with 'discount' reward scheme.
    """

    def step(self, action):
        obs, _reward, done, info = self.env.step(action)
        if done:
            progress = info["episode_progress"]
            attacker = info["episode_reward_attacker"]
            if progress != 0:
                reward = attacker / progress
            else:
                reward = 0
        else:
            reward = 0
        return obs, reward, done, info


class DenseRewardPerProgressWrapper(gym.Wrapper):
    """
    Mimics SparseRewardPerProgressWrapper but with dense rewards.
    The trick is to end the episode at a given target progress.
    This way, we know the divisor from SparseRewardPerProgressWrapper in advance.

    Normalized to episode reward 1.
    """

    def __init__(self, env, episode_len=None):
        super().__init__(env)

        self.drpb_max_progress = episode_len
        self.drpb_factor = 1 / self.drpb_max_progress

        for k in ["max_steps", "max_time", "max_progress"]:
            if k in self.env.core_kwargs.keys():
                self.env.core_kwargs.pop(k, None)
                warnings.warn(
                    f"DenseRewardPerProgressWrapper overwrites argument '{k}' given to wrapped env"
                )

        self.env.core_kwargs["max_steps"] = self.drpb_max_progress * 100
        self.env.core_kwargs["max_progress"] = self.drpb_max_progress

    def reset(self):
        self.drpb_acc = 0
        return self.env.reset()

    def step(self, action):
        obs, reward, done, info = self.env.step(action)

        reward *= self.drpb_factor
        self.drpb_acc += reward

        if done:
            got = info["episode_progress"]
            want = self.drpb_max_progress

            if got < want:
                warnings.warn(f"observed too little progress: {got}/{want}")

            if got > want * 1.1:
                warnings.warn(f"observed too much progress: {got}/{want}")

            # TODO I don't get why we observe got = want + 1 for nakamoto regularly.
            # For Ethereum, it's relatively clear, that we observe got > want:
            # the last block might include a couple of orphans and thereby
            # increment got by more than one. But this reasoning does not apply
            # for Nakamoto.

            # fix progress mismatch on last step
            if got != want:
                # want = 2 | 4
                # got = 3 | 6
                delta = want - got  # = -1 | -2
                fix = delta * self.drpb_acc / got  # = -1 * acc/3 | -2 * acc/6
                reward += fix

        return obs, reward, done, info


class ExtendObservationWrapper(gym.Wrapper):
    """
    Adds fields from info dict or elsewhere to the observation space.
    """

    def __init__(self, env, fields):
        super().__init__(env)
        self.eow_fields = fields
        self.eow_n = len(fields)
        low = numpy.zeros(self.eow_n)
        high = numpy.zeros(self.eow_n)
        for i in range(self.eow_n):
            _fn, l, h, _default = fields[i]
            low[i] = l
            high[i] = h
        low = numpy.append(self.observation_space.low, low)
        high = numpy.append(self.observation_space.high, high)
        self.observation_space = gym.spaces.Box(low, high, dtype=numpy.float64)

    def reset(self):
        raw_obs = self.env.reset()
        obs = numpy.zeros(self.eow_n)
        for i in range(self.eow_n):
            _fn, _low, _high, default = self.eow_fields[i]
            obs[i] = default
        return numpy.append(raw_obs, obs)

    def step(self, action):
        raw_obs, reward, done, info = self.env.step(action)
        obs = numpy.zeros(self.eow_n)
        for i in range(self.eow_n):
            f, _low, _high, _default = self.eow_fields[i]
            obs[i] = f(self, info)
        return numpy.append(raw_obs, obs), reward, done, info

    def policy(self, obs, name="honest"):
        obs = obs[: -self.eow_n]
        return self.env.policy(obs, name)


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


class ClearInfoWrapper(gym.Wrapper):
    """
    Deletes all info keys but the ones in `keep_keys`.

    Apply before vectorization to avoid IPC overhead.
    """

    def __init__(self, env, keep_keys=[]):
        super().__init__(env)
        self.ciw_keys = keep_keys

    def reset(self):
        return self.env.reset()

    def step(self, action):
        obs, reward, done, was_info = self.env.step(action)
        info = dict()
        for key in self.ciw_keys:
            if key in was_info.key():
                info[key] = was_info[key]
        return obs, reward, done, info