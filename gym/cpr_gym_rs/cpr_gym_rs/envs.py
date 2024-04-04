import gymnasium
import numpy
from . import _rust


class FC16SSZwPT(gymnasium.Env):
    def __init__(self, *args, **kwargs):
        self.rs_env = _rust.FC16SSZwPT(*args, **kwargs)
        self.action_space = gymnasium.spaces.Discrete(4)
        self.observation_space = gymnasium.spaces.Box(
            shape=(3,), low=0.0, high=1.0, dtype="float64"
        )

    def reset(self, *args, seed=None, options=None):
        super().reset(seed=seed)  # this will not work probably
        return self.rs_env.reset()

    def step(self, action):
        return self.rs_env.step(action)


class Generic(gymnasium.Env):
    protocols = {"nakamoto": lambda: _rust.Protocol.Nakamoto}

    def __init__(self, protocol, *args, alpha, gamma, horizon, max_blocks, **kwargs):
        p = Generic.protocols[protocol](*args, **kwargs)

        self.rs_env = _rust.GenericEnv(
            p, alpha=alpha, gamma=gamma, horizon=horizon, max_blocks=max_blocks
        )

        self.action_space = gymnasium.spaces.Box(
            shape=(1,), low=-127, high=127, dtype=numpy.int8
        )

        obs, _info = self.rs_env.reset()

        self.observation_space = gymnasium.spaces.Box(
            shape=obs.shape, low=0, high=2, dtype=numpy.uint8
        )

    def reset(self, *args, seed=None, options=None):
        super().reset(seed=seed)  # this will not work probably
        return self.rs_env.reset()

    def step(self, action):
        return self.rs_env.step(action[0])
