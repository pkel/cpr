import gym
from cpr_gym import specs
import numpy as np
from copy import deepcopy


class EnvWrapper:
    def __init__(self, spec):
        self.env = gym.make("cpr-v0", spec=spec)
        self.action_space = self.env.action_space
        self.observation_space = self.env.observation_space
        self.running_reward = 0
        self.state = None

    def reset(self):
        self.running_reward = 0
        self.state = self.env.reset()
        return {
            "obs": deepcopy(self.state),
            "action_mask": np.array([1, 1, 1], dtype=np.float32),
        }

    def step(self, action):
        obs, rew, done, info = self.env.step(action)
        self.running_reward += rew
        self.state = deepcopy(obs)
        score = self.running_reward if done else 0
        return (
            {"obs": obs, "action_mask": np.array([1, 1, 1], dtype=np.float32)},
            score,
            done,
            info,
        )

    def set_state(self, state):
        self.running_reward = state[1]
        self.env = deepcopy(state[0])
        obs = np.array(list(self.env.unwrapped.state))
        return {"obs": obs, "action_mask": np.array([1, 1, 1], dtype=np.float32)}

    def get_state(self):
        return deepcopy(self.env), self.running_reward
