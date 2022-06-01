import gym


class SparseRelativeRewardWrapper(gym.Wrapper):
    """
    Overwrites objective function.
    Calculates relative rewards at the end of the episode.
    One reward per episode.
    """

    def __init__(self, env):
        super().__init__(env)

    def reset(self):
        obs = self.env.reset()
        self.srrw_atk = 0
        self.srrw_def = 0
        return obs

    def step(self, action):
        obs, _reward, done, info = self.env.step(action)
        self.srrw_def += info["reward_defender"]
        self.srrw_atk += info["reward_attacker"]
        if done:
            sum = self.srrw_def + self.srrw_atk
            if sum != 0:
                reward = self.srrw_atk / sum
            else:
                reward = 0
        else:
            reward = 0
        return obs, reward, done, info
