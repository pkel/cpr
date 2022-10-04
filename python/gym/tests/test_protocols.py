import gym
from cpr_gym import protocols


def test_version():
    env = gym.make("cpr_gym:core-v0")
    assert isinstance(env.version, str)
    assert len(env.version) > 0


def test_default(capsys):
    env = gym.make("cpr_gym:core-v0")
    env.render()
    captured = capsys.readouterr().out.splitlines()[0]
    assert captured == "Nakamoto consensus; SSZ'16 attack space; α=0.25 attacker"


def test_config(capsys):
    env = gym.make(
        "cpr_gym:core-v0", proto=protocols.bk(k=8), alpha=0.33, gamma=0.1, defenders=10
    )
    env.render()
    captured = capsys.readouterr().out.splitlines()[0]
    assert captured == "Bₖ with k=8; SSZ'16-like attack space; α=0.33 attacker"
    assert env.puzzles_per_block() == 8


def test_policies_honest():
    env = gym.make(
        "cpr_gym:core-v0", proto=protocols.bk(k=8), alpha=0.33, gamma=0.2, defenders=2
    )
    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(env.policy(obs, "honest"))


def test_policies_selfish():
    env = gym.make(
        "cpr_gym:core-v0", proto=protocols.bk(k=8), alpha=0.33, gamma=0.5, defenders=3
    )
    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(env.policy(obs, "selfish"))


def test_nakamoto(capsys):
    env = gym.make(
        "cpr_gym:core-v0",
        proto=protocols.nakamoto(),
        alpha=0.33,
        gamma=0.7,
        defenders=5,
    )
    env.render()
    captured = capsys.readouterr().out.splitlines()[0]
    assert captured == "Nakamoto consensus; SSZ'16 attack space; α=0.33 attacker"
    assert env.puzzles_per_block() == 1

    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(env.policy(obs, "honest"))

    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(env.policy(obs, "eyal-sirer-2014"))


def test_ethereum(capsys):
    env = gym.make(
        "cpr_gym:core-v0",
        proto=protocols.ethereum(),
        alpha=0.13,
        gamma=0.9,
        defenders=10,
    )
    env.render()
    captured = capsys.readouterr().out.splitlines()[0]
    assert captured == (
        "Ethereum's adaptation of GHOST with height-preference, work-progress, and uncle cap 2; "
        "SSZ'16-like attack space; α=0.13 attacker"
    )
    assert env.puzzles_per_block() == 1


def test_bkll(capsys):
    env = gym.make(
        "cpr_gym:core-v0",
        proto=protocols.bkll(k=17),
        alpha=0.33,
        gamma=0.3,
        defenders=4,
    )
    env.render()
    captured = capsys.readouterr().out.splitlines()[0]
    assert captured == "Bₖ/ll with k=17; SSZ'16-like attack space; α=0.33 attacker"
    assert env.puzzles_per_block() == 17

    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(env.policy(obs, "honest"))

    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(env.policy(obs, "selfish"))


def test_tailstorm(capsys):
    env = gym.make(
        "cpr_gym:core-v0",
        proto=protocols.tailstorm(k=13, reward="discount"),
        alpha=0.33,
        gamma=0.8,
        defenders=5,
    )
    env.render()
    captured = capsys.readouterr().out.splitlines()[0]
    assert captured == (
        "Tailstorm with k=13, 'discount' rewards, and 'optimal' sub block selection; "
        "SSZ'16-like attack space; α=0.33 attacker"
    )
    assert env.puzzles_per_block() == 13

    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(env.policy(obs, "honest"))

    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(env.policy(obs, "override-catchup"))


def test_tailstormll(capsys):
    env = gym.make(
        "cpr_gym:core-v0",
        proto=protocols.tailstormll(k=13, reward="discount"),
        alpha=0.33,
        gamma=0.8,
        defenders=5,
    )
    env.render()
    captured = capsys.readouterr().out.splitlines()[0]
    assert captured == (
        "Tailstorm/ll with k=13, 'discount' rewards, and 'optimal' sub block selection; "
        "SSZ'16-like attack space; α=0.33 attacker"
    )
    assert env.puzzles_per_block() == 13

    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(env.policy(obs, "honest"))

    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(env.policy(obs, "override-catchup"))
