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


class MapRewardWrapper(gym.Wrapper):
    """
    Applies the given function to all rewards.
    The given function takes to inputs, reward and info dictionary.
    """

    def __init__(self, env, fn):
        super().__init__(env)
        self.mrw_fn = fn

    def step(self, action):
        obs, reward, done, info = self.env.step(action)
        reward = self.mrw_fn(reward, info)
        return obs, reward, done, info


class AssumptionScheduleWrapper(gym.Wrapper):
    """
    Reconfigures the assumptions about the attacker (alpha and gamma) on each reset.
    Extends observation space with current assumptions.
    Reports alpha and gamma in the info dictionary.
    Use this for learning generic policies that can handle different assumptions.
    Supports showing other assumptions to the agent than assumed.
    """

    def __init__(
        self, env, alpha=None, gamma=None, pretend_alpha=None, pretend_gamma=None
    ):
        super().__init__(env)

        if callable(alpha):
            self.asw_alpha_fn = alpha
        else:
            try:
                alpha_iterator = itertools.cycle(alpha)
                self.asw_alpha_fn = lambda: next(alpha_iterator)
            except TypeError:
                self.asw_alpha_fn = lambda: alpha

        if callable(gamma):
            self.asw_gamma_fn = gamma
        else:
            try:
                gamma_iterator = itertools.cycle(gamma)
                self.asw_gamma_fn = lambda: next(gamma_iterator)
            except TypeError:
                self.asw_gamma_fn = lambda: gamma

        self.asw_pretend_alpha = pretend_alpha
        self.asw_pretend_gamma = pretend_gamma

        # extend observation space
        low = self.observation_space.low
        high = self.observation_space.high
        low = numpy.append(low, [0.0, 0.0])
        high = numpy.append(high, [1.0, 1.0])
        self.observation_space = gym.spaces.Box(low, high, dtype=numpy.float64)

    def observation(self, obs):
        assumptions = [self.asw_alpha, self.asw_gamma]
        if self.asw_pretend_alpha is not None:
            assumptions[0] = float(self.asw_pretend_alpha)
        if self.asw_pretend_gamma is not None:
            assumptions[1] = float(self.asw_pretend_gamma)
        return numpy.append(obs, assumptions)

    # overwrite core env's policy
    def policy(self, obs, name="honest"):
        obs = obs[:-2]
        return self.env.policy(obs, name)

    def reset(self):
        self.asw_alpha = self.asw_alpha_fn()
        self.asw_gamma = self.asw_gamma_fn()
        self.env.core_kwargs["alpha"] = self.asw_alpha
        self.env.core_kwargs["gamma"] = self.asw_gamma

        obs = self.env.reset()
        obs = AssumptionScheduleWrapper.observation(self, obs)
        return obs

    def step(self, action):
        obs, reward, done, info = self.env.step(action)
        info["alpha"] = self.asw_alpha
        info["gamma"] = self.asw_gamma
        obs = AssumptionScheduleWrapper.observation(self, obs)
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
