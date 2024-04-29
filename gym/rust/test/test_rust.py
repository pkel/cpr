from cpr_gym_rs import _rust
import random
import pytest


def test_FC16SSZwPT():
    env = _rust.FC16SSZwPT(alpha=0.5, gamma=0.5, horizon=25)
    for _ in range(1000):
        a = random.randint(0, env.n_actions() - 1)
        #  print("State:", env)
        #  print("Action:", env.describe_action(a))
        obs, rew, term, trunc, info = env.step(a)
        #  print("Step:", obs, rew, term, trunc, info)
        if term or trunc:
            env.reset()


def test_actions():
    proto = _rust.Protocol.Nakamoto
    env = _rust.GenericEnv(proto, alpha=0.5, gamma=0.5, horizon=25)

    assert env.encode_action_continue() == 0.0
    assert env.encode_action_release(0) < 0.0
    assert env.encode_action_consider(0) > 0.0

    # action encoding must be injective
    monotonic = env.encode_action_release(255)
    for i in range(254, -1, -1):
        next_a = env.encode_action_release(i)
        assert monotonic < next_a, f"Release {i}"
        monotonic = next_a
    for i in range(0, 256):
        next_a = env.encode_action_consider(i)
        assert monotonic < next_a, f"Consider {i}"
        monotonic = next_a

    # fail when out of bounds
    with pytest.raises(Exception):
        env.encode_action_release(256)
    with pytest.raises(Exception):
        env.encode_action_consider(256)

    # check rust side decoding

    assert env.describe_action(env.encode_action_continue()) == "Continue"
    assert env.describe_action(-0.1) == "Continue"
    assert env.describe_action(0.1) == "Continue"
    assert env.describe_action(0.0) == "Continue"

    # first value should work
    assert env.describe_action(env.encode_action_release(0)) == "Release(0)"
    assert env.describe_action(env.encode_action_consider(0)) == "Consider(0)"
    # last value that should work
    assert env.describe_action(env.encode_action_release(255)) == "Release(255)"
    assert env.describe_action(env.encode_action_consider(255)) == "Consider(255)"
    # the others are easy
    for i in range(0, 256):
        assert env.describe_action(env.encode_action_release(i)) == f"Release({i})"
        assert env.describe_action(env.encode_action_consider(i)) == f"Consider({i})"

    # sensible out of bounds handling
    assert env.describe_action(-1) == "Release(255)"
    assert env.describe_action(1) == "Consider(255)"
    with pytest.raises(BaseException):
        assert env.describe_action(1.01)
    with pytest.raises(BaseException):
        assert env.describe_action(-1.01)


def test_nakamoto():
    proto = _rust.Protocol.Nakamoto
    env = _rust.GenericEnv(proto, alpha=0.5, gamma=0.5, horizon=25)
    for _ in range(1000):
        a = random.uniform(-1, 1)
        #  print("State:", env)
        #  print("Action:", env.describe_action(a))
        obs, rew, term, trunc, info = env.step(a)
        #  print("Step:", obs, rew, term, trunc, info)
        if term or trunc:
            env.reset()
