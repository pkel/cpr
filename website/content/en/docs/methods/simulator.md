---
title: "Simulator"
description: |
  How we simulate protocol executions.
lead: |
  How we simulate protocol executions.
draft: false
images: []
menu:
  docs:
    parent: "method"
weight: 230
toc: true
---

Having talked about our [model for virtual protocol
executions](../virtual-environment) and about [how we specify
protocols](../protocol-specification), we can proceed with the
description of the network simulator. Like before, we use Python as
pseudocode to describe important details. The code snippets are
optimized for readability not speed. The real simulator is implemented
in a compiled language to achieve good performance.

Input 1: Network topology

* Graph. Vertices are nodes, edges are links, maybe `neighbours(src)`
* Edge defines propagation delay, optionally in the form of a
probability distribution.
* We'll need some notation for that, maybe Python function `delay(src,
dst)` returning seconds.
* Optionally flooding, i.e., nodes forward blocks as they become locally
visible.

Input 2: protocol & nodes

* Blockchain spec: `roots`, `validity` form protocol spec
* Per node: functions `init`, `update`, `mining`. Might be different
from protocol spec. Maybe function `node(i)` returning triple.

Input 3: proof-of-work

* mining rate $\lambda$ in puzzles per second or activation delay
$\lambda^{-1}$ in seconds
* weights $\kappa_.$.

State:

* Global DAG.
* State for each node.
* Block-visibility for each node.

Time:

(implementation detail, nice to know but maybe not fully describe this
here)

* Discrete event simulation
* Time-ordered queue of future events
* Infinite loop consuming & handling first event in the queue
* Event handler might queue future events
* Time between events is skipped
* Single non-blocking process

Appends:

Communication:

Proof-of-work:
