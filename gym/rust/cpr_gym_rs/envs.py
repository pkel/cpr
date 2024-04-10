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

    def __init__(self, protocol, *args, alpha, gamma, horizon, **kwargs):
        p = Generic.protocols[protocol](*args, **kwargs)

        self.rs_env = _rust.GenericEnv(p, alpha=alpha, gamma=gamma, horizon=horizon)

        self.action_space = gymnasium.spaces.Discrete(
            255, start=-127
        )  # does not work with sb3 DQN/MLP
        self.action_space = gymnasium.spaces.Discrete(255)  # TODO seed

        obs, _info = self.rs_env.reset()

        self.observation_space = gymnasium.spaces.Box(
            shape=obs.shape, low=float("-inf"), high=float("inf"), dtype=numpy.float32
        )

    def reset(self, *args, seed=None, options=None):
        super().reset(seed=seed)  # this will not work probably
        return self.rs_env.reset()

    def step(self, action):
        return self.rs_env.step(action - 127)
