import gym
import numpy as np
import engine
import specs


class Core(gym.Env):
    metadata = {"render.modes": ["ascii"]}

    def __init__(self, spec=specs.default):
        self.env = engine.create(spec)
        self.action_space = gym.spaces.Discrete(engine.n_actions(self.env))
        low = engine.observation_low(self.env)
        high = engine.observation_high(self.env)
        self.observation_space = gym.spaces.Box(low, high, dtype=np.float64)

    def reset(self):
        return engine.reset(self.env)

    def step(self, a):
        return engine.step(self.env, a)

    def render(self, mode="ascii"):
        print(engine.to_string(self.env))

    def policies(self):
        return engine.policies(self.env)
