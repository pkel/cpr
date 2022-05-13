import numpy as np

import gym
from cpr_gym import protocols


def test_version():
    env = gym.make("cpr-v0")
    assert isinstance(env.version, str)
    assert len(env.version) > 0


def test_default(capsys):
    env = gym.make("cpr-v0")
    env.render()
    captured = capsys.readouterr().out.splitlines()[0]
    assert captured == "Protocol Nakamoto against α=0.25 attacker"


def test_config(capsys):
    env = gym.make(
        "cpr-v0", proto=protocols.bk(k=8), alpha=0.33, gamma=0.1, defenders=10
    )
    env.render()
    captured = capsys.readouterr().out.splitlines()[0]
    assert captured == "Protocol Bₖ with k=8 against α=0.33 attacker"


def test_policies_honest():
    env = gym.make(
        "cpr-v0", proto=protocols.bk(k=8), alpha=0.33, gamma=0.2, defenders=2
    )
    p = env.policies["honest"]
    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(p(np.array(obs)))


def test_policies_selfish():
    env = gym.make(
        "cpr-v0", proto=protocols.bk(k=8), alpha=0.33, gamma=0.5, defenders=3
    )
    p = env.policies["selfish"]
    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(p(np.array(obs)))


def test_nakamoto(capsys):
    env = gym.make(
        "cpr-v0", proto=protocols.nakamoto(), alpha=0.33, gamma=0.7, defenders=5
    )
    env.render()
    captured = capsys.readouterr().out.splitlines()[0]
    assert captured == "Protocol Nakamoto against α=0.33 attacker"

    p = env.policies["honest"]
    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(p(np.array(obs)))

    p = env.policies["eyal-sirer-2014"]
    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(p(np.array(obs)))


def test_bk_ll(capsys):
    env = gym.make(
        "cpr-v0", proto=protocols.bk_ll(k=17), alpha=0.33, gamma=0.3, defenders=4
    )
    env.render()
    captured = capsys.readouterr().out.splitlines()[0]
    assert captured == "Protocol Bₖ/ll with k=17 against α=0.33 attacker"

    p = env.policies["honest"]
    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(p(np.array(obs)))

    p = env.policies["selfish"]
    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(p(np.array(obs)))


def test_george(capsys):
    env = gym.make(
        "cpr-v0",
        proto=protocols.george(k=13, reward="discount"),
        alpha=0.33,
        gamma=0.8,
        defenders=5,
    )
    env.render()
    captured = capsys.readouterr().out.splitlines()[0]
    assert captured == "Protocol George with k=13 against α=0.33 attacker"

    p = env.policies["honest"]
    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(p(np.array(obs)))

    p = env.policies["override_block"]
    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(p(np.array(obs)))
