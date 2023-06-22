# Consensus Protocol Research

CPR is a toolbox for specifying, simulating, and attacking proof-of-work
consensus protocols. In this repository you find
- protocol specifications for Bitcoin, Ethereum PoW, and others,
- implementations of known attacks against these protocols,
- a simulator that executes the specified protocols and attacks in a
  virtual environment,
- tooling for automatic attack-search with reinforcement learning (RL) and
- evaluation scripts and notebooks for the above.

I'm working on [a website](https://pkel.github.io/cpr/) with more details.

##### Related Work

- CPR was inspired by previous work on HotPoW and Parallel Proof-of-Work
  / $\mathcal B_k$.
  [[code]](https://github.com/pkel/hotpow)
  [[preprint]](https://arxiv.org/abs/1907.13531)
  [[AFT'22 paper]](https://arxiv.org/abs/2204.00034)
- We applied CPR to analyze the Tailstorm consensus and cryptocurrency.
  [[preprint]](https://arxiv.org/abs/2306.12206)


## Python/RL Quickstart

CPR provides an OpenAI Gym environment for attack search with Python RL
frameworks. If you meet the following **requirements**, you can install
it from PyPI.

- Unix-like operating system with x86_64 support
- CPython, version >= 3.9

```shell
pip install cpr-gym
```

If this worked, you are ready to go. The following snippet simulates
2016 steps of honest behaviour in Nakamoto consensus.

```python
import gym
import cpr_gym

env = gym.make("cpr-nakamoto-v0", episode_len = 2016)
obs = env.reset()
done = False
while not done:
    action = env.policy(obs, "honest")
    obs, rew, done, info = env.step(action)
```

## Install from Source

The protocol specifications and simulator are OCaml programs. Also most
parts of the Gym environment are written in OCaml. The Python module
`cpr_gym` loads the OCaml code from a pre-compiled shared object named
`cpr_gym_engine.so`. In order to install the package from source, you
have to build this shared object and hence have the OCaml toolchain
installed.

**Opam** is the OCaml package manager. It's a bit like Python's `pip` or
Javascript's `npm`. We use it do download and install our OCaml
dependencies and to manage different versions of the OCaml compiler.
Make sure that a recent version (>= 2.0) is installed on your system.
[Follow these instructions](https://opam.ocaml.org/doc/Install.html).
Then use `make setup` to get compiler and dependencies setup in the
current working directory under `_opam`. Later, e.g. when dependencies
change, run `make dependencies` to update the toolchain. If you ever
suspect that the OCaml dependencies are broken, and you do not know how
to fix it, delete the `_opam` directory and run `make setup` again.

**Dune** is an OCaml build system. We use it to build executables and
shared objects, and to run tests. You do not have to interact with dune
directly. Just run `make build` to test whether the OCaml build works.

Now, installing `cpr_gym` as editable Python package should work. Try
`pip install -e .` and follow the short Python example above. If it
works, you're ready to go.

`import cpr_gym` tries to detect editable installs. If so,
`cpr_gym_engine.so` is loaded from the OCaml build directory
(`./_build`). You can rebuild the DLL with `make build`.

It might be useful to install all Python development dependencies with
`pip install -r requirements.txt`. Afterwards, you can run the full test
suite, OCaml and Python, with `make test`.
