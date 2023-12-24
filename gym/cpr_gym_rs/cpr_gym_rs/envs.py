import gymnasium
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
