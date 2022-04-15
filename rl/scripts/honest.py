import numpy as np

import gym
from cpr_gym import specs
from stable_baselines3 import A2C
from tqdm import tqdm

env = gym.make("cpr-v0", spec=specs.nakamoto(alpha=0.35, n_steps=100))
# model = A2C("MlpPolicy", env, verbose=1)
# model.learn(total_timesteps=10000)
p = env.policies()["honest"]
sum_rs = []
sum_attacker_rs = []
sum_defender_rs = []
relative_rs = []
actions = []
_is = []
for _ in tqdm(range(1)):
    obs = env.reset()
    rs = []
    attacker_rs = []
    defender_rs = []
    done = False
    i = 0
    while not done:
        action = p(np.array(obs))
        obs, r, done, info = env.step(0)
        rs.append(r)
        attacker_rs.append(info["reward_attacker"])
        defender_rs.append(info["reward_defender"])
        actions.append(action)
        i += 1
    _is.append(i)
    sum_rs.append(np.sum(rs))
    sum_attacker_rs.append(np.sum(attacker_rs))
    sum_defender_rs.append(np.sum(defender_rs))
    relative_rs.append(
        np.sum(attacker_rs) / (np.sum(defender_rs) + np.sum(attacker_rs))
    )
unique, unique_counts = np.unique(actions, return_counts=True)
unique_counts = dict(zip(unique, unique_counts))
print(f"Average reward: {np.mean(sum_rs)}, Std: {np.std(sum_rs)}")
print(f"Average relative reward: {np.mean(relative_rs)}, Std: {np.std(relative_rs)}")
print(f"Unique actions: {unique_counts}")
print(f"Average steps: {np.mean(_is)}, Std: {np.std(_is)}")

"""
Average reward: 36925.33272892762, Std: 38343.528967470455
Average relative reward: 0.4902039926007399, Std: 0.004918660639838942
Unique actions: {0: 1000000}
"""
