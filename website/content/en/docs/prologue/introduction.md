---
title: "Introduction"
description: |
  CPR provides tools for specifying, simulating, and attacking
  proof-of-work consensus protocols like Bitcoin and Ethereum.
lead: |
  CPR provides tools for specifying, simulating, and attacking
  proof-of-work consensus protocols like Bitcoin and Ethereum.
draft: false
images: []
menu:
  docs:
    parent: "prologue"
weight: 100
toc: true
---

## Work in Progress

The whole website is under construction. There's not much information
for newcomers yet. At some point I want to provide tutorials the
following use-cases.

- Evaluate specified protocols in a virtual environment.
- Evaluate supported attacks in a virtual environment.
- Search attacks against specified protocols using reinforcement learning.
- Specification of protocols.
- Specification of attack spaces.

If you want to start now, don't hesitate to contact
[me](https://github.com/pkel) for support.

## Overview

CPR’s core component is a network simulation engine for proof-of-work
protocols. The engine takes a protocol specification and a network
topology as input, then simulates the execution of the protocol in the
network over time. Additional tooling enables automated search for
attack strategies with reinforcement learning.

I'm working on a detailed documentation such that my tooling becomes
useful for others. So far, I've documented most parts of the core
simulation engine, most of the specified protocols, and some useful
network topologies.

The [virtual enviroment page]({{< method "virtual-environment" >}})
informally describes CPR's approach to network simulation. The [protocol
specification page]({{< method "protocol-specification"
>}}) describes how we specify protocols. The [simulator page]({{< method
"simulator" >}}) describes how the simulator executes the specified
protocols in a virtual network. Read these pages in order, to learn
about the inner workings of CPR. Then, continue exploring the different
[proof-of-work consensus protocols]({{< protocol "overview" >}}).

## Python/RL Quickstart

CPR provides an OpenAI Gym environment for attack search with Python RL
frameworks. You can install it PyPI, if you meet the following
**requirements**.

- Unix-like operating system with x86_64 support. I've tested Linux
  (Fedora and Ubuntu) and Mac OSX.
- CPython, version >= 3.9
- `pip` package installer for Python.

In a terminal, run `pip install cpr-gym`. Then, you are ready to run the
following Python snippet which simulates 2016 steps (mined or received
blocks) of honest behaviour in [Nakamoto consensus]({{<protocol
"nakamoto">}}).

```python
import gym
import cpr_gym

env = gym.make("cpr-nakamoto-v0", episode_len=2016)
obs = env.reset()
done = False
while not done:
    action = env.policy(obs, "honest")
    obs, rew, done, info = env.step(action)
```

Until I catch up with the documentation, consider reading [our
tests](https://github.com/pkel/cpr/tree/master/gym/tests) to find out
how to attack the other protocols and how to set important parameters
like the attacker capabilities [alpha and gamma]({{< post
"emulating-gamma" >}}).

If the PyPI package does not work for you, consult the [GitHub
project](https://github.com/pkel/cpr) for instructions on building the
project from source.
