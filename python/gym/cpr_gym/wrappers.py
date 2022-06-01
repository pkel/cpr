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


class SparseRewardPerBlockWrapper(gym.Wrapper):
    """
    Overwrites objective function.
    Calculates reward per confirmed proof-of-work puzzle solution at the end of episode.
    One reward per episode.

    This is similar to the SparseRelativeRewardWrapper. The two wrappers are
    equivalent for protocols or reward functions that assign a constant reward
    per confirmed puzzle solution. For different reward functions, e.g.
    Tailstorm's 'discount' scheme, this wrapper is better suited.
    """

    def __init__(self, env):
        super().__init__(env)

    def reset(self):
        obs = self.env.reset()
        self.srpbw_acc = 0
        self.srpbw_pow = 0
        return obs

    def step(self, action):
        obs, _reward, done, info = self.env.step(action)
        self.srpbw_acc += info["reward_attacker"]
        self.srpbw_pow += info["reward_n_pows"]
        if done:
            if self.srpbw_pow != 0:
                reward = self.srpbw_acc / self.srpbw_pow
            else:
                reward = 0
        else:
            reward = 0
        return obs, reward, done, info
