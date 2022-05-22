import gym
import numpy as np
import engine
import protocols
import random
import warnings


class Core(gym.Env):
    metadata = {"render.modes": ["ascii"]}

    def __init__(self, proto=protocols.nakamoto(), **kwargs):
        self.env = engine.create(proto, **kwargs)
        self.action_space = gym.spaces.Discrete(engine.n_actions(self.env))
        low = engine.observation_low(self.env)
        low = np.array(low)  # for pickling; why doesn't pyml support pickling?
        high = engine.observation_high(self.env)
        high = np.array(high)
        self.observation_space = gym.spaces.Box(low, high, dtype=np.float64)
        self.version = engine.cpr_lib_version
        self.policies = engine.policies(self.env)

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


class RandomAlpha(Core):
    def __init__(self, proto, **kwargs):
        if "alpha" in kwargs.keys():
            kwargs.pop("alpha", None)
            warnings.warn("spurious argument: alpha")

        super().__init__(proto, **kwargs)
        self.proto = proto
        self.kwargs = kwargs

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

    def extend_obs(self, obs):
        return {"core": obs, "extra": np.array([self.alpha], dtype=np.float64)}

    def reset(self):
        self.alpha = random.uniform(0, 0.5)
        self.env = engine.create(self.proto, alpha=self.alpha, **self.kwargs)
        obs = super().reset()
        obs = self.extend_obs(obs)
        return obs

    def step(self, a):
        obs, r, d, i = super().step(a)
        obs = self.extend_obs(obs)
        return obs, r, d, i


class RingBuffer:
    def __init__(self, n, init=0.0, dtype=np.float64):
        self.buf = np.full(n, init, dtype=dtype)
        self.n = n
        self.i = 0

    def write(self, x):
        self.buf[self.i] = x
        self.i += 1
        if self.i >= self.n:
            self.i = 0


class Wip(Core):
    def __init__(self, proto, target_block_interval=1, **kwargs):
        if "activation_delay" in kwargs.keys():
            kwargs.pop("activation_delay", None)
            warnings.warn("spurious argument: activation_delay")

        if "alpha" in kwargs.keys():
            kwargs.pop("alpha", None)
            warnings.warn("spurious argument: alpha")

        super().__init__(proto, **kwargs)
        self.proto = proto
        self.target_block_interval = target_block_interval
        self.kwargs = kwargs

        # extend observation space
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

        # init ring buffers for DAA and reporting
        daa_buf_size = 25
        self.rb_alpha = RingBuffer(daa_buf_size, 0)
        self.rb_activation_delay = RingBuffer(daa_buf_size, target_block_interval)
        self.rb_observed_block_interval = RingBuffer(
            daa_buf_size, target_block_interval
        )
        self.rb_reward = RingBuffer(daa_buf_size, 0)

    def extend_obs(self, obs):
        return {"core": obs, "extra": np.array([self.alpha], dtype=np.float64)}

    def reset(self):
        # sample alpha
        self.alpha = random.uniform(0, 0.5)
        # estimate difficulty
        i = np.abs(self.rb_alpha.buf - self.alpha).argmin()
        ad = self.rb_activation_delay.buf[i]
        obi = self.rb_observed_block_interval.buf[i]
        self.activation_delay = ad * self.target_block_interval / obi
        # create env with new parameters
        self.env = engine.create(
            self.proto,
            alpha=self.alpha,
            activation_delay=self.activation_delay,
            **self.kwargs
        )
        # reset env
        obs = super().reset()
        # reset episode-scoped counter
        self.episode_pow_confirmed = 0
        self.episode_reward = 0
        # extend observation space
        obs = self.extend_obs(obs)
        return obs

    def step(self, a):
        obs, reward, done, info = super().step(a)
        # count confirmed puzzle solutions
        self.episode_pow_confirmed += info["reward_n_pows"]
        # accumulate reward
        self.episode_reward += reward
        if done:
            # calculate observed block interval
            obi = info["simulator_clock_now"] / self.episode_pow_confirmed
            # correct for DAA mismatch
            error = self.target_block_interval / obi - 1
            extra = self.episode_reward * error
            reward += extra
            self.episode_reward += extra
            # record data in ring buffers
            self.rb_alpha.write(self.alpha)
            self.rb_activation_delay.write(self.activation_delay)
            self.rb_observed_block_interval.write(obi)
            self.rb_reward.write(self.episode_reward)
            # report a few metrics
            info["alpha"] = self.alpha
            info["activation_delay"] = self.activation_delay
            info["episode_reward"] = self.episode_reward
            info["observed_block_interval"] = obi
            info["daa_error"] = error
            info["daa_extra_reward"] = extra
        # extend observation space
        obs = self.extend_obs(obs)
        return obs, reward, done, info
