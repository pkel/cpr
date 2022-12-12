# Consensus Protocol Research

CPR is a toolbox for specifying, simulating, and attacking proof-of-work
consensus protocols. In this repository you find
- protocol specifications for Bitcoin, Ethereum PoW, and others,
- implementations of known attacks against these protocols,
- a simulator that executes the specified protocols and attacks in a,
  virtual environment,
- tooling for automatic attack-search with reinforcement learning (RL) and
- evaluation scripts and notebooks for the above.

[A website](https://cpr-preview.netlify.app) with more details is currently work-in-progress.

## Python/RL Quickstart

CPR provides an OpenAI Gym environment for attack search with Python RL
frameworks. It will soon be published on PyPI. Until then, you can
download and install the package manually.
1. Find the [latest release on Github](https://github.com/pkel/cpr/releases/latest).
2. Open the Assets and download the appropriate wheel for your platform.
   - Mac OSX: select `*-macosx-*.whl`
   - Linux: select `*-manylinux-*.whl`
3. Install the wheel with `pip install cpr_gym-*.whl`.

You are now ready to use the Gym environment.

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
parts of the Gym environment are written in OCaml. The Python package
`cpr_gym` loads the OCaml code from a pre-compiled shared object named
`cpr_gym_engine.so`. In order to build the package from source, you have
to build this shared object and hence have the OCaml toolchain
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

The editable install will automatically invoke `dune` to keep the
`cpr_gym_engine.so` up to date. If you change the OCaml code for the
simulator, these changes will be reflected in the Python gym
environment. Updating the `cpr_gym_engine.so` happens during `import`,
so you'll have to restart your kernel if you work from a notebook.

It might be useful to install all Python development dependencies with
`pip install -r requirements.txt`. Afterwards, you can run the full test
suite, OCaml and Python, with `make test`.
