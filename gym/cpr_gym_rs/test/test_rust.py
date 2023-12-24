from cpr_gym_rs import _rust
from random import randint


def test_FC16SSZwPT():
    env = _rust.FC16SSZwPT(alpha=0.25, gamma=0.5, horizon=25)
    for _ in range(10):
        a = randint(0, env.n_actions() - 1)
        #  print("State:", env)
        #  print("Action:", env.describe_action(a))
        obs, rew, term, trunc, info = env.step(a)
        #  print("Step:", obs, rew, term, trunc, info)
        if term or trunc:
            env.reset()
