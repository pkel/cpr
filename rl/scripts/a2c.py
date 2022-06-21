import numpy as np

import gym
from cpr_gym import specs
from stable_baselines3 import A2C, PPO, DQN
from tqdm import tqdm

ALPHA = 0.4


env = gym.make("cpr-v0", spec=specs.nakamoto(alpha=ALPHA))
model = A2C("MlpPolicy", env, verbose=1)
model.learn(total_timesteps=10e5)
model.save(f"a2c_nakamoto_alpha_{ALPHA}")
# p = env.policies()["honest"]
# obs = env.reset()
# rs = []
# for x in range(600):
#     obs, r, done, info = env.step(p(np.array(obs)))
#     rs.append(r)
# print(np.sum(rs))
sum_rs = []
sum_attacker_rs = []
sum_defender_rs = []
relative_rs = []
actions = []
for i in tqdm(range(100)):
    done = False
    rs = []
    attacker_rs = []
    defender_rs = []
    obs = env.reset()
    while not done:
        action, _state = model.predict(obs, deterministic=True)
        obs, reward, done, info = env.step(action)
        rs.append(reward)
        attacker_rs.append(info["reward_attacker"])
        defender_rs.append(info["reward_defender"])
        actions.append(action)
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
"""
Average reward: 46700.37878937397, Std: 176872.9906061578
Average relative reward: 0.48934404059594044, Std: 0.005359245320997703
Unique actions: {0: 489368, 1: 510632}
"""
