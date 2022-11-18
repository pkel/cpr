import engine
import gym
import numpy as np
import protocols
from . import wrappers


class Core(gym.Env):
    metadata = {"render.modes": ["ascii"]}

    def __init__(self, proto=protocols.nakamoto(), **kwargs):
        self.core_kwargs = kwargs
        self.core_kwargs["proto"] = proto

        self.ocaml_env = None
        Core.reset(self)  # sets self.ocaml_env from self.core_kwargs

        self.action_space = gym.spaces.Discrete(engine.n_actions(self.ocaml_env))
        low = engine.observation_low(self.ocaml_env)
        low = np.array(low)  # for pickling; why doesn't pyml support pickling?
        high = engine.observation_high(self.ocaml_env)
        high = np.array(high)  # for pickling; why doesn't pyml support pickling?
        self.observation_space = gym.spaces.Box(low, high, dtype=np.float64)

        self.version = engine.cpr_lib_version

    def policies(self):
        return engine.policies(self.ocaml_env).keys()

    def policy(self, obs, name="honest"):
        try:
            return engine.policies(self.ocaml_env)[name](obs)
        except KeyError:
            raise ValueError(
                name
                + " is not a valid policy; choose from "
                + ", ".join(self.policies())
            )

    def reset(self):
        # TODO / ocaml: we could expose engine.init that combines create and reset
        self.ocaml_env = engine.create(**self.core_kwargs)
        obs = engine.reset(self.ocaml_env)
        obs = np.array(obs)  # for pickling; why doesn't pyml support pickling?
        return obs

    def step(self, a):
        obs, r, d, i = engine.step(self.ocaml_env, a)
        obs = np.array(obs)  # for pickling; why doesn't pyml support pickling?
        return obs, r, d, i

    def render(self, mode="ascii"):
        print(engine.to_string(self.ocaml_env))


def env_fn(**config):
    protocol_fn = getattr(protocols, config.get("protocol", "nakamoto"))
    protocol_args = config.get("protocol_args", {})

    episode_len = config.get("episode_len", 128)

    alpha = config.get("alpha", 0.33)
    gamma = config.get("gamma", 0.5)
    if "defenders" in config:
        defenders = config["defenders"]
    else:
        defenders = np.ceil((1 - alpha) / (1 - gamma))

    rewards = dict(
        sparse_relative=(
            wrappers.SparseRelativeRewardWrapper,
            dict(max_steps=episode_len),
        ),
        sparse_per_progress=(
            wrappers.SparseRewardPerProgressWrapper,
            dict(max_steps=episode_len),
        ),
        dense_per_progress=(
            lambda env: wrappers.DenseRewardPerProgressWrapper(
                env, episode_len=episode_len
            ),
            dict(),
        ),
    )

    reward_wrapper, env_args = rewards[config.get("reward", "sparse_relative")]

    env = Core(
        proto=protocol_fn(**protocol_args),
        alpha=alpha,
        gamma=gamma,
        defenders=defenders,
        **env_args
    )

    env = reward_wrapper(env)

    if config.get("normalize_reward", True):
        env = wrappers.MapRewardWrapper(env, lambda r: r / alpha)

    return env
