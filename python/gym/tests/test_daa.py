import gym
from cpr_gym import protocols


def test_simple_daa():
    target = 600  # block interval
    eps = 30

    def env_with_activation_delay(x):
        env = gym.make(
            "cpr_gym:core-v0",
            proto=protocols.nakamoto(),
            max_steps=10000,
            alpha=1 / 3,
            gamma=0.5,
            defenders=2,
            activation_delay=x,
        )
        p = env.policies()["sapirshtein-2016-sm1"]
        return (env, p)

    # run simulation assuming 100% efficiency (= 0% orphan rate)
    env, p = env_with_activation_delay(target)
    obs = env.reset()
    done = False
    n_pow = 0
    while not done:
        obs, _, done, info = env.step(p(obs))
        n_pow += info["reward_n_pows"]
    observed = info["simulator_clock_rewarded"] / n_pow  # block interval

    # the selfish mining policy causes orphans, hence block interval should be out of tolerance
    assert not target - eps < observed < target + eps

    # re-run simulation with corrected difficulty
    env, p = env_with_activation_delay(target * target / observed)
    obs = env.reset()
    done = False
    n_pow = 0
    while not done:
        obs, _, done, info = env.step(p(obs))
        n_pow += info["reward_n_pows"]
    observed = info["simulator_clock_rewarded"] / n_pow  # block interval

    # observed block interval should be within tolerance now
    assert target - eps < observed < target + eps


def test_max_time():
    target = 10000.0
    env = gym.make(
        "cpr_gym:core-v0",
        proto=protocols.nakamoto(),
        max_time=target,
        max_steps=int(target * 2),  # set high enough such that max_time takes effect
        activation_delay=1,
    )
    p = env.policies()["honest"]

    obs = env.reset()
    done = False
    while not done:
        obs, _, done, info = env.step(p(obs))

    assert info["simulator_clock_now"] >= target
    assert info["simulator_clock_rewarded"] >= target - 10
