import gym
from cpr_gym import protocols
from stable_baselines3.common.env_checker import check_env


def test_version():
    env = gym.make("cpr_gym:core-v0", max_progress=42)
    assert isinstance(env.version, str)
    assert len(env.version) > 0


def test_default(capsys):
    env = gym.make("cpr_gym:core-v0", max_steps=1000)
    env.render()
    captured = capsys.readouterr().out.splitlines()[0]
    assert captured == (
        "Nakamoto consensus; "
        "SSZ'16 attack space with unit observations; α=0.25 attacker"
    )


def test_policies_honest():
    env = gym.make(
        "cpr_gym:core-v0",
        proto=protocols.bk(k=8, reward="constant", unit_observation=True),
        alpha=0.33,
        gamma=0.2,
        defenders=2,
        max_steps=10000,
    )
    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(env.policy(obs, "honest"))


def test_policies_selfish():
    env = gym.make(
        "cpr_gym:core-v0",
        proto=protocols.bk(k=8, reward="constant", unit_observation=True),
        alpha=0.33,
        gamma=0.5,
        defenders=3,
        max_steps=10000,
    )
    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(env.policy(obs, "minor-delay"))


def test_nakamoto(capsys):
    env = gym.make(
        "cpr_gym:core-v0",
        proto=protocols.nakamoto(unit_observation=True),
        alpha=0.33,
        gamma=0.7,
        defenders=5,
        max_steps=10000,
    )
    env.render()
    captured = capsys.readouterr().out.splitlines()[0]
    assert captured == (
        "Nakamoto consensus; "
        "SSZ'16 attack space with unit observations; α=0.33 attacker"
    )

    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(env.policy(obs, "honest"))

    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(env.policy(obs, "eyal-sirer-2014"))

    check_env(env)


def test_ethereum(capsys):
    env = gym.make(
        "cpr_gym:core-v0",
        proto=protocols.ethereum(reward="discount", unit_observation=True),
        alpha=0.13,
        gamma=0.9,
        defenders=10,
        max_steps=10000,
    )
    env.render()
    captured = capsys.readouterr().out.splitlines()[0]
    assert captured == (
        "Ethereum with heaviest_chain-preference, work-progress, uncle cap 2, "
        "and discount-rewards; "
        "SSZ'16-like attack space with unit observations; α=0.13 attacker"
    )

    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(env.policy(obs, "honest"))

    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(env.policy(obs, "selfish_discard"))

    check_env(env)


def test_bk(capsys):
    env = gym.make(
        "cpr_gym:core-v0",
        proto=protocols.bk(k=42, reward="constant", unit_observation=True),
        alpha=0.33,
        gamma=0.3,
        defenders=4,
        max_steps=10000,
    )
    env.render()
    captured = capsys.readouterr().out.splitlines()[0]
    assert captured == (
        "Bₖ with k=42 and constant rewards; "
        "SSZ'16-like attack space with unit observations; α=0.33 attacker"
    )

    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(env.policy(obs, "honest"))

    obs = env.reset()
    for x in range(600):
        obs, _, _, info = env.step(env.policy(obs, "minor-delay"))

    assert info["protocol_k"] == 42

    check_env(env)


def test_spar(capsys):
    env = gym.make(
        "cpr_gym:core-v0",
        proto=protocols.spar(k=17, reward="constant", unit_observation=True),
        alpha=0.33,
        gamma=0.3,
        defenders=4,
        max_steps=10000,
    )
    env.render()
    captured = capsys.readouterr().out.splitlines()[0]
    assert captured == (
        "Simple Parallel PoW with k=17 and constant rewards; "
        "SSZ'16-like attack space with unit observations; α=0.33 attacker"
    )

    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(env.policy(obs, "honest"))

    obs = env.reset()
    for x in range(600):
        obs, _, _, info = env.step(env.policy(obs, "selfish"))

    assert info["protocol_k"] == 17

    check_env(env)


def test_stree(capsys):
    env = gym.make(
        "cpr_gym:core-v0",
        proto=protocols.stree(
            k=13, reward="discount", subblock_selection="optimal", unit_observation=True
        ),
        alpha=0.33,
        gamma=0.8,
        defenders=5,
        max_steps=10000,
    )
    env.render()
    captured = capsys.readouterr().out.splitlines()[0]
    assert captured == (
        "Simple Parallel PoW with tree-style voting, k=13, discount rewards, "
        "and optimal sub-block selection; "
        "SSZ'16-like attack space with unit observations; α=0.33 attacker"
    )

    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(env.policy(obs, "honest"))

    obs = env.reset()
    for x in range(600):
        obs, _, _, info = env.step(env.policy(obs, "override-catchup"))

    assert info["protocol_k"] == 13

    check_env(env)


def test_tailstorm(capsys):
    env = gym.make(
        "cpr_gym:core-v0",
        proto=protocols.tailstorm(
            k=13,
            reward="discount",
            subblock_selection="heuristic",
            unit_observation=True,
        ),
        alpha=0.33,
        gamma=0.8,
        defenders=5,
        max_steps=10000,
    )
    env.render()
    captured = capsys.readouterr().out.splitlines()[0]
    assert captured == (
        "Tailstorm with k=13, discount rewards, and heuristic sub-block selection; "
        "SSZ'16-like attack space with unit observations; α=0.33 attacker"
    )

    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(env.policy(obs, "honest"))

    obs = env.reset()
    for x in range(600):
        obs, _, _, info = env.step(env.policy(obs, "avoid-loss"))

    assert info["protocol_k"] == 13

    check_env(env)


def test_tailstormjune(capsys):
    env = gym.make(
        "cpr_gym:core-v0",
        proto=protocols.tailstormjune(k=7, reward="discount", unit_observation=False),
        alpha=0.25,
        gamma=0.7,
        defenders=4,
        max_steps=10000,
    )
    env.render()
    captured = capsys.readouterr().out.splitlines()[0]
    assert captured == (
        "Tailstorm/ll (June '22 version) with k=7 and discount rewards; "
        "SSZ'16-like attack space with raw observations; α=0.25 attacker"
    )

    obs = env.reset()
    for x in range(600):
        obs, _, _, _ = env.step(env.policy(obs, "honest"))

    obs = env.reset()
    for x in range(600):
        obs, _, _, info = env.step(env.policy(obs, "override-catchup"))

    assert info["protocol_k"] == 7

    check_env(env)
