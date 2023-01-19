import gym
import joblib
import os
import stable_baselines3

memory = joblib.Memory(os.path.dirname(__file__) + "/_cache", verbose=0)

# Keep models in memory
# I _guess_ this is not needed for our small models
#
# loaded = {}
#
# def load(path):
#     if path in loaded.keys():
#         return loaded[path]
#     else:
#         m = stable_baselines3.PPO.load(path)
#         loaded[path] = m
#         return m


def observe_episode(info):
    r = dict()
    for k, v in info.items():
        if k.startswith("episode_"):
            r[k] = v
    return r


@memory.cache
def measure_trained(env_name, path, seed=None, **kwargs):
    env = gym.make(env_name, **kwargs)
    model = stable_baselines3.PPO.load(path)
    obs = env.reset()
    done = False
    while not done:
        action, _ = model.predict(obs, deterministic=True)
        obs, _reward, done, info = env.step(action)
    return observe_episode(info)


@memory.cache
def measure_hardcoded(env_name, policy, seed=None, **kwargs):
    env = gym.make(env_name, **kwargs)
    obs = env.reset()
    done = False
    while not done:
        action = env.policy(obs, policy)
        obs, reward, done, info = env.step(action)
    return observe_episode(info)
