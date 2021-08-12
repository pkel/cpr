import numpy as np

import gym
from cpr_gym import specs


def test_default(capsys):
    env = gym.make("cpr-v0")
    env.render()
    captured = capsys.readouterr().out.splitlines()[0]
    assert captured == "Protocol Bₖ with k=51 against α=0.25 attacker"


def test_spec_arg(capsys):
    env = gym.make("cpr-v0", spec=specs.bk(k=8, alpha=0.33))
    env.render()
    captured = capsys.readouterr().out.splitlines()[0]
    assert captured == "Protocol Bₖ with k=8 against α=0.33 attacker"


def test_policies_honest():
    env = gym.make("cpr-v0", spec=specs.bk(k=8, alpha=0.33))
    p = env.policies()["honest"]
    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(p(np.array(obs)))


def test_policies_selfish():
    env = gym.make("cpr-v0", spec=specs.bk(k=8, alpha=0.33))
    p = env.policies()["selfish"]
    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(p(np.array(obs)))
