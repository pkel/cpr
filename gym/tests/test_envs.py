import gym
from cpr_gym import wrappers

import random
from stable_baselines3.common.env_checker import check_env


def run_episode(env, policy):
    obs = env.reset()
    done = False
    while not done:
        obs, rew, done, info = env.step(env.policy(obs, policy))
    return obs, rew, done, info


def fuzz_episode(env):
    obs = env.reset()
    done = False
    while not done:
        obs, rew, done, info = env.step(env.action_space.sample())
    return obs, rew, done, info


def test_coreEnv():
    env = gym.make("cpr_gym:core-v0", max_steps=2016)
    check_env(env)
    run_episode(env, "honest")
    fuzz_episode(env)


def test_sparseRelativeRewardWrapper():
    env = gym.make("cpr_gym:core-v0", max_steps=32)
    env = wrappers.SparseRelativeRewardWrapper(env)
    check_env(env)
    for i in range(42):
        run_episode(env, "honest")
        fuzz_episode(env)


def test_sparseRewardPerProgressWrapper():
    env = gym.make("cpr_gym:core-v0", max_steps=32)
    env = wrappers.SparseRewardPerProgressWrapper(env)
    check_env(env)
    for i in range(42):
        run_episode(env, "honest")
        fuzz_episode(env)


def test_denseRewardPerProgressWrapper():
    env = gym.make("cpr_gym:core-v0", max_progress=float("inf"))
    env = wrappers.DenseRewardPerProgressWrapper(env, episode_len=32)
    check_env(env)
    for i in range(42):
        run_episode(env, "honest")
        fuzz_episode(env)


def test_assumptionScheduleWrapper():
    env = gym.make("cpr_gym:core-v0", max_steps=32)
    env = wrappers.AssumptionScheduleWrapper(env, alpha=0.33, gamma=0.1)
    check_env(env)
    for i in range(2):
        obs, _, _, i = fuzz_episode(env)
        assert i["alpha"] == 0.33
        assert i["gamma"] == 0.1
        assert obs[-2] == 0.33
        assert obs[-1] == 0.1

    env = wrappers.AssumptionScheduleWrapper(
        env, alpha=[0.1, 0.2, 0.3], gamma=[0.1, 0.5, 0.9]
    )
    check_env(env)
    alphas = dict()
    gammas = dict()
    for i in range(10):
        obs, _, _, i = run_episode(env, "honest")
        assert i["alpha"] == obs[-2]
        assert i["gamma"] == obs[-1]
        alphas[i["alpha"]] = True
        gammas[i["gamma"]] = True
    assert len(alphas.keys()) <= 3
    assert len(gammas.keys()) <= 3

    env = wrappers.AssumptionScheduleWrapper(
        env,
        alpha=lambda: random.uniform(0.1, 0.5),
        gamma=lambda: random.uniform(0.0, 1.0),
    )
    check_env(env)
    alphas = dict()
    gammas = dict()
    for i in range(42):
        obs, _, _, i = run_episode(env, "honest")
        assert i["alpha"] == obs[-2]
        assert i["gamma"] == obs[-1]
        alphas[i["alpha"]] = True
        gammas[i["gamma"]] = True
    assert len(alphas.keys()) > 30
    assert len(gammas.keys()) > 30

    env = wrappers.AssumptionScheduleWrapper(
        env,
        alpha=lambda: random.uniform(0.1, 0.5),
        gamma=lambda: random.uniform(0.0, 1.0),
        pretend_alpha=0.33,
        pretend_gamma=0.33,
    )
    check_env(env)
    for i in range(42):
        obs, _, _, i = run_episode(env, "honest")
        assert obs[-2] == 0.33
        assert obs[-1] == 0.33
        alphas[i["alpha"]] = True
        gammas[i["gamma"]] = True
    assert len(alphas.keys()) > 30
    assert len(gammas.keys()) > 30


def test_ExtendObservationWrapper():
    env = gym.make("cpr_gym:core-v0", max_progress=100)
    was_n = len(env.observation_space.low)

    fields = []
    fields.append(((lambda self, info: info["episode_progress"]), 0, float("inf"), 0))
    env = wrappers.ExtendObservationWrapper(env, fields)

    n = len(env.observation_space.low)
    assert n == was_n + len(fields)

    check_env(env)


def test_EpisodeRecorderWrapper():
    env = gym.make("cpr_gym:core-v0", max_progress=100)
    env = wrappers.EpisodeRecorderWrapper(env, n=10, info_keys=["head_height"])
    check_env(env)
    for i in range(42):
        run_episode(env, "honest")
    assert len(env.erw_history) == 10
    for entry in env.erw_history:
        assert "episode_reward" in entry.keys()
        assert "head_height" in entry.keys()


def test_registered_envs(capsys):
    env = gym.make("cpr_gym:cpr-nakamoto-v0")
    env.render()
    captured = capsys.readouterr().out.splitlines()[0]
    assert captured == "Nakamoto consensus; SSZ'16 attack space; α=0.45 attacker"

    env = gym.make("cpr_gym:cpr-tailstorm-v0")
    env.render()
    captured = capsys.readouterr().out.splitlines()[0]
    assert captured == (
        "Tailstorm with k=8, discount rewards, and "
        "heuristic sub-block selection; "
        "SSZ'16-like attack space; α=0.45 attacker"
    )

    env = gym.make(
        "cpr_gym:cpr-tailstorm-v0",
        alpha=lambda: random.uniform(0, 0.5),
        gamma=lambda: random.uniform(0, 0.9),
        pretend_alpha=0.45,
        pretend_gamma=0.5,
    )
    check_env(env)
