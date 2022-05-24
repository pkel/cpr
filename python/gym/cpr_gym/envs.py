import engine
import gym
import numpy as np
import pandas as pd
import protocols
import random
import warnings


class Core(gym.Env):
    metadata = {"render.modes": ["ascii"]}

    def __init__(self, proto=protocols.nakamoto(), **kwargs):
        self.env = engine.create(proto=proto, **kwargs)
        self.action_space = gym.spaces.Discrete(engine.n_actions(self.env))
        low = engine.observation_low(self.env)
        low = np.array(low)  # for pickling; why doesn't pyml support pickling?
        high = engine.observation_high(self.env)
        high = np.array(high)
        self.observation_space = gym.spaces.Box(low, high, dtype=np.float64)
        self.version = engine.cpr_lib_version

    def policies(self):
        return engine.policies(self.env)

    def reset(self):
        obs = engine.reset(self.env)
        obs = np.array(obs)  # for pickling; why doesn't pyml support pickling?
        return obs

    def step(self, a):
        obs, r, d, i = engine.step(self.env, a)
        obs = np.array(obs)  # for pickling; why doesn't pyml support pickling?
        return obs, r, d, i

    def render(self, mode="ascii"):
        print(engine.to_string(self.env))


class Auto(Core):
    def __init__(
        self,
        proto=protocols.nakamoto(),
        alpha_min=0.1,
        alpha_max=1,
        target_runtime=128,
        target_block_interval=1,
        buf_size=128,
        daa_window=2016,
        **kwargs,
    ):
        # filter arguments which are valid for Core but not for this env
        for k in ["activation_delay", "alpha", "max_steps", "max_time"]:
            if k in kwargs.keys():
                kwargs.pop(k, None)
                warnings.warn(f"unused argument '{k}'")

        # check compatibility of alpha_min and target runtime
        if target_block_interval * target_runtime * alpha_min < 10:
            warnings.warn(
                "weak attackers won't solve many puzzles per episode "
                "causing high reward variance. Consider increasing 'alpha_min' "
                "or 'target_runtime', or decreasing 'target_block_interval'"
            )

        self.alpha_min = alpha_min
        self.alpha_max = alpha_max
        self.target_runtime = target_runtime
        self.target_block_interval = target_block_interval
        self.buf_size = buf_size
        self.daa_window = daa_window

        self.core_kwargs = kwargs
        self.core_kwargs["proto"] = proto
        self.core_kwargs["max_time"] = target_runtime
        self.core_kwargs["max_steps"] = target_runtime * 10

        # initialize Core env to get observation spaces
        super().__init__(**self.core_kwargs)
        # extend observation space with alpha
        self.extend_observation_space()
        # initialize ring buffers for DAA and reporting
        self.init_ring_buffer()

    # extend observation space with alpha

    def extend_observation_space(self):
        self.observation_space = gym.spaces.Dict(
            {
                "core": self.observation_space,
                "extra": gym.spaces.Box(
                    np.array([0], dtype=np.float64),
                    np.array([1], dtype=np.float64),
                    dtype=np.float64,
                ),
            }
        )

    def extend_observation(self, obs):
        return {"core": obs, "extra": np.array([self.alpha], dtype=np.float64)}

    def policies(self):
        return {k: lambda obs: p(obs["core"]) for k, p in super().policies().items()}

    # ring buffers for DAA and reporting

    def init_ring_buffer(self):
        self.buf_loc = 0
        self.buf = pd.DataFrame(
            data=dict(
                alpha=[
                    random.uniform(self.alpha_min, self.alpha_max)
                    for _ in range(self.buf_size)
                ],
                activation_delay=[
                    self.target_block_interval for _ in range(self.buf_size)
                ],
                observed_block_interval=[
                    self.target_block_interval for _ in range(self.buf_size)
                ],
                reward=[0 for _ in range(self.buf_size)],
            )
        )
        # calculate DAA window
        buffer_describes = (
            self.buf_size * self.target_runtime / self.target_block_interval
        )  # block intervals
        if self.daa_window > buffer_describes:
            warnings.warn(
                "buffer too small to support given 'daa_window'."
                "DAA will consider the entire buffer."
            )
        self.daa_alpha_eps = (
            (self.alpha_max - self.alpha_min) * self.daa_window / buffer_describes / 2
        )

    def write_ring_buffer(
        self, alpha, activation_delay, observed_block_interval, reward
    ):
        self.buf.iloc[self.buf_loc] = [
            alpha,
            activation_delay,
            observed_block_interval,
            reward,
        ]
        self.buf_loc += 1
        if self.buf_loc >= self.buf_size:
            self.buf_loc = 0

    # DAA

    def estimate_difficulty(self, alpha):
        i = np.nonzero(np.abs(self.buf.alpha.values - alpha) < self.daa_alpha_eps)[0]
        if np.size(i) < 1:  # pick closest alpha if empty
            i = np.abs(self.buf.alpha.values - alpha).argmin()
        ad = self.buf.activation_delay.values[i]
        obi = self.buf.observed_block_interval.values[i]
        return np.mean(ad * self.target_block_interval / obi), np.size(i)

    # overwrite step and reset

    def reset(self):
        # sample alpha and estimate difficulty
        self.alpha = random.uniform(self.alpha_min, self.alpha_max)
        self.activation_delay, self.daa_input_episodes = self.estimate_difficulty(
            self.alpha
        )
        # create env with new parameters
        self.env = engine.create(
            alpha=self.alpha,
            activation_delay=self.activation_delay,
            **self.core_kwargs,
        )
        # reset Core env
        obs = super().reset()
        # reset episode-scoped counters
        self.episode_pow_confirmed = 0
        self.episode_reward = 0
        # extend observation space
        obs = self.extend_observation(obs)
        return obs

    def step(self, a):
        obs, reward, done, info = super().step(a)
        # normalize reward; honest episode reward is 1; high variance for small alphas
        reward = reward / self.alpha / self.target_runtime
        # reduced variance for small alphas
        # reward = (1 - self.alpha + reward) / self.target_runtime
        # count confirmed puzzle solutions
        self.episode_pow_confirmed += info["reward_n_pows"]
        # accumulate reward
        self.episode_reward += reward
        if done:
            # we want the episode to run a fixed amount of time
            # the DAA should get it about right
            # we correct the remaining error here
            now = info["simulator_clock_rewarded"]
            error = self.target_runtime / now - 1
            extra = self.episode_reward * error
            reward += extra
            self.episode_reward += extra
            # observe block interval
            obi = now / self.episode_pow_confirmed
            # record data in ring buffer
            self.write_ring_buffer(
                self.alpha, self.activation_delay, obi, self.episode_reward
            )
            # report a few metrics
            info["alpha"] = self.alpha
            info["activation_delay"] = self.activation_delay
            info["episode_reward"] = self.episode_reward
            info["observed_runtime"] = now
            info["observed_block_interval"] = obi
            info["daa_error"] = error
            info["daa_extra_reward"] = extra
            info["daa_input_episodes"] = self.daa_input_episodes
        # extend observation space
        obs = self.extend_observation(obs)
        return obs, reward, done, info
