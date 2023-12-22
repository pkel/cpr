import gymnasium.utils.env_checker
from cpr_gym_rs import FC16SSZwPT
from random import randint

env = FC16SSZwPT(alpha=0.25, gamma=0.5, horizon=25)

for _ in range(10):
    a = randint(0, env.n_actions() - 1)
    print("State:", env)
    print("Action:", env.describe_action(a))
    obs, rew, term, trunc, info = env.step(a)
    print("Step:", obs, rew, term, trunc, info)
    if term or trunc:
        env.reset()


class Env(gymnasium.Env):
    def __init__(self, *args, **kwargs):
        self.rs_env = FC16SSZwPT(*args, **kwargs)
        self.action_space = gymnasium.spaces.Discrete(4)
        self.observation_space = gymnasium.spaces.Box(
            shape=(3,), low=0.0, high=1.0, dtype="float64"
        )

    def reset(self, *args, seed=None, options=None):
        super().reset(seed=seed)  # this will not work probably
        return self.rs_env.reset()

    def step(self, action):
        return self.rs_env.step(action)


for _ in range(10):
    gymnasium.utils.env_checker.check_env(
        Env(alpha=0.25, gamma=0.5, horizon=25), skip_render_check=True
    )
