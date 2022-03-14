import numpy as np

import gym
from cpr_gym import specs
from stable_baselines3 import A2C
from tqdm import tqdm
import matplotlib.pyplot as plt
import seaborn as sns


env = gym.make("cpr-v0", spec=specs.nakamoto(alpha=0.35))
# model = A2C("MlpPolicy", env, verbose=1)
# model.learn(total_timesteps=10000)
p = env.policies()["selfish"]
sum_rs = []
sum_attacker_rs = []
sum_defender_rs = []
relative_rs = []
actions = []
_is = []
for _ in tqdm(range(100)):
    obs = env.reset()
    rs = []
    attacker_rs = []
    defender_rs = []
    done = False
    i = 0
    while not done:
        action = p(np.array(obs))
        obs, r, done, info = env.step(action)
        rs.append(r)
        attacker_rs.append(info["reward_attacker"])
        defender_rs.append(info["reward_defender"])
        actions.append(action)
        i += 1
    sum_rs.append(np.sum(rs))
    sum_attacker_rs.append(np.sum(attacker_rs))
    sum_defender_rs.append(np.sum(defender_rs))
    relative_rs.append(
        np.sum(attacker_rs) / (np.sum(defender_rs) + np.sum(attacker_rs))
    )
    _is.append(i)
unique, unique_counts = np.unique(actions, return_counts=True)
unique_counts = dict(zip(unique, unique_counts))
print(f"Average reward: {np.mean(sum_rs)}, Std: {np.std(sum_rs)}")
print(f"Average relative reward: {np.mean(relative_rs)}, Std: {np.std(relative_rs)}")
print(f"Unique actions: {unique_counts}")
print(f"Average steps: {np.mean(_is)}, Std: {np.std(_is)}")
fig, ax = plt.subplots()
sns.distplot(sum_rs, ax=ax)
plt.show()
"""
Average reward: 2721.0071208567556, Std: 150.17074259685518
Average relative reward: 0.5960710882917148, Std: 0.012208569807294437
Unique actions: {1: 533091, 3: 466909}
"""
