import engine
import gym
import numpy as np
import protocols
import warnings
from . import wrappers


class Core(gym.Env):
    metadata = {"render.modes": ["ascii"]}

    def __init__(
        self,
        proto=protocols.nakamoto(),
        alpha=0.25,
        gamma=0.5,
        activation_delay=1.0,
        **kwargs,
    ):
        self.core_kwargs = kwargs
        self.core_kwargs["proto"] = proto
        self.core_kwargs["alpha"] = alpha
        self.core_kwargs["gamma"] = gamma
        self.core_kwargs["activation_delay"] = activation_delay

        if (
            "max_time" not in kwargs
            and "max_progress" not in kwargs
            and "max_steps" not in kwargs
        ):
            raise ValueError(
                "cpr_gym: set at least one of kwargs max_progress, max_steps, and max_time."
            )

        for k in ["max_time", "max_progress", "max_steps"]:
            if k in kwargs and kwargs[k] is None:
                # some use cases set max_* later from a wrapper
                # to pass the check above they can set max_something=None
                # without this loop here, max_something=None would yield a
                # type error
                kwargs.pop(k)

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
        kwargs = self.core_kwargs.copy()
        d = kwargs.pop("defenders", None)
        if d is None:
            a = kwargs["alpha"]
            g = kwargs["gamma"]
            if g >= 1:
                raise ValueError("gamma must be smaller than 1")
            d = int(np.ceil((1 - a) / (1 - g)))
            if d >= 100:
                warnings.warn(
                    f"Expensive assumptions: alpha={a} and gamma={g} imply defenders={d}"
                )

        self.ocaml_env = engine.create(defenders=d, **kwargs)
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
    pretend_alpha=None,
    pretend_gamma=None,
    defenders=None,
    reward="sparse_relative",
    normalize_reward=True,
):
    protocol_fn = getattr(protocols, protocol)

    if protocol_args is None:
        protocol_args = _protocol_args
    else:
        protocol_args = _protocol_args | protocol_args

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
            dict(max_steps=None),
        ),
    )

    reward_wrapper, env_args = rewards[reward]

    env = Core(
        proto=protocol_fn(**protocol_args),
        alpha=0.0,  # set from wrapper below
        gamma=0.0,  # set from wrapper below
        defenders=defenders,
        **env_args,
    )

    env = wrappers.AssumptionScheduleWrapper(
        env,
        alpha=alpha,
        gamma=gamma,
        pretend_alpha=pretend_alpha,
        pretend_gamma=pretend_gamma,
    )

    env.reset()  # set alpha and gamma from wrapper

    env = reward_wrapper(env)

    if normalize_reward:
        env = wrappers.MapRewardWrapper(env, lambda r, i: r / i["alpha"])

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
