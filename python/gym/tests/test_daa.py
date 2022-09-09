import collections
import gym
import numpy
from cpr_gym import protocols


def test_simple_daa():
    target = 600  # block interval
    eps = 25

    def env_with_activation_delay(x):
        env = gym.make(
            "cpr_gym:core-v0",
            proto=protocols.nakamoto(),
            max_steps=100,
            alpha=1 / 3,
            gamma=0.5,
            defenders=2,
            activation_delay=x,
        )

        def p(obs):
            return env.policy(obs, "sapirshtein-2016-sm1")

        return (env, p)

    # run simulation assuming 100% efficiency (= 0% orphan rate)
    env, p = env_with_activation_delay(target)
    obs = env.reset()
    done = False
    while not done:
        obs, _, done, info = env.step(p(obs))
    observed = info["episode_chain_time"] / info["episode_progress"]

    # the selfish mining policy causes orphans, hence block interval should be out of tolerance
    assert not target - eps < observed < target + eps

    # re-run simulation with corrected difficulty
    ad = collections.deque([target], maxlen=100)
    ct = collections.deque([info["episode_chain_time"]], maxlen=100)
    pr = collections.deque([info["episode_progress"]], maxlen=100)
    for _ in range(0, 200):
        next_ad = target * numpy.mean(
            numpy.array(ad) / numpy.array(ct) * numpy.array(pr)
        )
        ad.append(next_ad)
        env, p = env_with_activation_delay(next_ad)
        obs = env.reset()
        done = False
        while not done:
            obs, _, done, info = env.step(p(obs))
        ct.append(info["episode_chain_time"])
        pr.append(info["episode_progress"])
        observed = numpy.sum(ct) / numpy.sum(pr)
        #  print(next_ad, observed)

    # observed block interval should be within tolerance now
    assert target - eps < observed < target + eps
    #  assert false


def test_max_time():
    target = 42 * 10
    env = gym.make(
        "cpr_gym:core-v0",
        proto=protocols.nakamoto(),
        max_time=target,
        max_steps=int(target * 2),  # set high enough such that max_time takes effect
        activation_delay=1,
    )

    obs = env.reset()
    done = False
    while not done:
        obs, _, done, info = env.step(env.policy(obs, "honest"))

    assert info["episode_chain_time"] >= target - 10
