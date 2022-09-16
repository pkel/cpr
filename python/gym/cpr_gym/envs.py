import engine
import gym
import numpy as np
import protocols


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

    def __deepcopy__(self, memo):
        r = Core(**self.core_kwargs)
        # OCaml side env is stateful, this is quite expensive
        r.ocaml_env = engine.copy(self.ocaml_env)
        return r

    def policies(self):
        return engine.policies(self.ocaml_env).keys()

    def policy(self, obs, name="honest"):
        return engine.policies(self.ocaml_env)[name](obs)

    def puzzles_per_block(self):
        return engine.puzzles_per_block(self.ocaml_env)

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
