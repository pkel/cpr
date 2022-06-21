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


def test_coreEnv():
    env = gym.make("cpr_gym:core-v0", max_steps=2016)
    check_env(env)
    run_episode(env, "honest")


def test_sparseRelativeRewardWrapper():
    env = gym.make("cpr_gym:core-v0", max_steps=32)
    env = wrappers.SparseRelativeRewardWrapper(env)
    check_env(env)
    for i in range(42):
        run_episode(env, "honest")


def test_sparseRewardPerBlockWrapper():
    env = gym.make("cpr_gym:core-v0", max_steps=32)
    env = wrappers.SparseRewardPerBlockWrapper(env)
    check_env(env)
    for i in range(42):
        run_episode(env, "honest")


def test_alphaScheduleWrapper():
    env = gym.make("cpr_gym:core-v0", max_steps=32)
    env = wrappers.AlphaScheduleWrapper(env, alpha_schedule=[0.33])
    check_env(env)
    for i in range(2):
        obs, _, _, i = run_episode(env, "honest")
        assert i["alpha"] == 0.33
        assert obs[-1] == 0.33

    env = wrappers.AlphaScheduleWrapper(env, alpha_schedule=[0.1, 0.2, 0.3])
    check_env(env)
    alphas = dict()
    for i in range(10):
        obs, _, _, i = run_episode(env, "honest")
        assert i["alpha"] == obs[-1]
        alphas[i["alpha"]] = True
    assert len(alphas.keys()) <= 3

    env = wrappers.AlphaScheduleWrapper(
        env, alpha_schedule=lambda: random.uniform(0.1, 0.5)
    )
    check_env(env)
    alphas = dict()
    for i in range(42):
        obs, _, _, i = run_episode(env, "honest")
        assert i["alpha"] == obs[-1]
        alphas[i["alpha"]] = True
    assert len(alphas.keys()) > 30
