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


gym.envs.register(id="core-v0", entry_point=Core)


def env_fn(
    protocol="nakamoto",
    protocol_args=None,
    _protocol_args=dict(),
    episode_len=128,
    alpha=0.45,
    gamma=0.5,
    defenders=None,
    reward="sparse_relative",
    normalize_reward=True,
):
    protocol_fn = getattr(protocols, protocol)

    if protocol_args is None:
        protocol_args = _protocol_args
    else:
        protocol_args = _protocol_args | protocol_args

    if defenders is None:
        defenders = int(np.ceil((1 - alpha) / (1 - gamma)))

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

    reward_wrapper, env_args = rewards[reward]

    env = Core(
        proto=protocol_fn(**protocol_args),
        alpha=alpha,
        gamma=gamma,
        defenders=defenders,
        **env_args
    )

    env = reward_wrapper(env)

    if normalize_reward:
        env = wrappers.MapRewardWrapper(env, lambda r: r / alpha)

    return env


gym.envs.register(id="cpr-v0", entry_point=env_fn)

gym.envs.register(
    id="cpr-nakamoto-v0",
    entry_point=env_fn,
    kwargs=dict(
        protocol="nakamoto",
        _protocol_args=dict(),
        reward="sparse_relative",
    ),
)

gym.envs.register(
    id="cpr-tailstorm-v0",
    entry_point=env_fn,
    kwargs=dict(
        protocol="tailstorm",
        _protocol_args=dict(k=8, reward="discount", subblock_selection="heuristic"),
        reward="sparse_per_progress",
    ),
)
