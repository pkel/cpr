import numpy as np

import gym
from cpr_gym import specs, engine
from stable_baselines3 import A2C, PPO, DQN
from tqdm import tqdm
import matplotlib.pyplot as plt
import seaborn as sns

ALPHA = 0.25
env = gym.make("cpr-v0", spec=specs.nakamoto(alpha=ALPHA))

model = PPO.load(f"ppo_nakamoto_alpha_{ALPHA}")

sum_rs = []
sum_attacker_rs = []
sum_defender_rs = []
relative_rs = []
actions = []
_is = []
for _ in tqdm(range(100)):
    done = False
    rs = []
    attacker_rs = []
    defender_rs = []
    _actions = []
    tds = []
    i = 0
    obs = env.reset()
    while not done:
        action, _state = model.predict(np.array(obs), deterministic=True)
        next_obs, reward, done, info = env.step(action)
        rs.append(reward)
        attacker_rs.append(info["reward_attacker"])
        defender_rs.append(info["reward_defender"])
        tds.append(info["reward_time_elapsed"])
        _actions.append(action)
        i += 1
        obs = next_obs
    _is.append(i)
    actions += _actions
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
fig, ax = plt.subplots()
sns.distplot(sum_rs, ax=ax)
plt.show()

# Average reward: 48464.505278942146, Std: 111065.6392517102

"""
Sum of rewards: 176471.09737369884
Sum of Attacker rewards: 4948.0
Sum of Defender rewards: 5052.0
Actions: {0: 4877, 1: 5123}
"""
