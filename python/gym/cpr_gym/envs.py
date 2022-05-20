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
