import cpr_gym
import gym
import random


def self_check():
    envs = [
        gym.make("cpr-v0", episode_len=10000),
        gym.make("cpr-nakamoto-v0", episode_len=10000),
        gym.make("cpr-tailstorm-v0", episode_len=10000),
    ]
    for env in envs:
        print(".", end="", flush=True)
        obs = env.reset()
        done = False
        while not done:
            action = env.policy(obs, "honest")
            obs, rew, done, info = env.step(action)


if __name__ == "__main__":
    print("Run self check .", end="", flush=True)
    self_check()
    print(" OK")
