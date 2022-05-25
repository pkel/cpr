import gym
from stable_baselines3.common.env_checker import check_env


def test_core():
    env = gym.make("cpr_gym:core-v0")
    check_env(env)
    obs = env.reset()
    for i in range(2016):
        obs, rew, done, info = env.step(env.policy(obs, "honest"))
        if done:
            obs = env.reset()


def test_auto():
    env = gym.make("cpr_gym:auto-v0")
    check_env(env)
    obs = env.reset()
    for i in range(2016):
        obs, rew, done, info = env.step(env.policy(obs, "honest"))
        if done:
            obs = env.reset()
