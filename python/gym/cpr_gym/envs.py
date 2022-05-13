import gym
import numpy as np
import engine
import protocols


class Core(gym.Env):
    metadata = {"render.modes": ["ascii"]}

    def __init__(self, proto=protocols.nakamoto(), **kwargs):
        self.env = engine.create(proto, **kwargs)
        self.action_space = gym.spaces.Discrete(engine.n_actions(self.env))
        low = engine.observation_low(self.env)
        high = engine.observation_high(self.env)
        self.observation_space = gym.spaces.Box(low, high, dtype=np.float64)
        self.version = engine.cpr_lib_version
        self.policies = engine.policies(self.env)

    def reset(self):
        return engine.reset(self.env)

    def step(self, a):
        return engine.step(self.env, a)

    def render(self, mode="ascii"):
        print(engine.to_string(self.env))
