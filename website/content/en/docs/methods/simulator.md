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

{{< alert icon="👉" text="WIP. You are looking at an unfinished page." />}}

Having talked about our [model for virtual protocol
executions](../virtual-environment) and about [how we specify
protocols](../protocol-specification), we can proceed with the
description of the network simulator. Like before, we use Python as
pseudocode to describe important details. The code snippets are
optimized for readability not speed. The real simulator is implemented
in a compiled language to achieve good performance.

## Inputs

`n: int` defines the number of nodes, `range(n)` addresses the
individual nodes.

`neighbours(i: int) -> int list` defines the outgoing network
connections of each node. If node `42` decides to share a block, the
simulator will send this block to the nodes returned by
`neighbours(42)`.

`message_delay(src: int, dst: int) -> float` defines the message
propagation delays. If node `42` sends a block to node `7` at time `t`,
the simulator will deliver the block to node `7` at time `t +
message_delay(42, 7)`. The function does not have to be
deterministic; it can model probability distributions. If the function
returns `float('inf')` the message will not be delivered.

`roots` and `validity` define the structure of the blockchain as
discussed in [the section on protocol
specifications](../protocol-specification). The simulator rejects
invalid blocks.

`node(i: int) -> tuple[init, update, mining]` defines the behaviour of nodes
as discussed in the [the section on protocol
specifications](../protocol-specification). In the simplest case, when
all nodes are honest, `node(i)` for any `i` returns the tree functions
`init`, `update`, and `mining` as defined in the protocol specification.
In attack scenarios, `node(i)` returns nonconforming functions for the
malicious nodes.

`mining_delay() -> float` and `select_miner() -> int` model
proof-of-work difficulty and hash-rates of the nodes. `mining_delay()`
defines the time between consecutive mining events. It is typically a
random function returning independent and exponentially distributed
values. `select_miner()` defines which node is successful at a
particular mining event. It is typically a random function which selects
nodes based on their relative hash-rate.

### Example

We provide an intuition about the simulator inputs by walking through a
realistic (but still simplified) example scenario. We consider [Nakamoto
consensus]({{< protocol "nakamoto" >}}) as it is deployed in Bitcoin.
The following table shows mining statistics for the last 7 days before
Dec. 14 2022. We got it from
[blockchain.com](https://www.blockchain.com/explorer/charts/pools).

| Miner / Pool | Relative Hash-Rate | Blocks Mined |
| ------------ | ------------------ | ------------ |
| Foundry USA  | 24,817%            | 271          |
| AntPool      | 20,147%            | 220          |
| F2Pool       | 15,110%            | 165          |
| Binance Pool | 13,736%            | 150          |
| ViaBTC       | 10,440%            | 114          |
| Braiins Pool | 5,311%             | 58           |
| Poolin       | 2,473%             | 27           |
| BTC.com      | 1,923%             | 21           |
| Luxor        | 1,648%             | 18           |
| Mara Pool    | 1,465%             | 16           |
| Ultimus      | 0,641%             | 7            |
| SBI Crypto   | 0,641%             | 7            |
| BTC M4       | 0,366%             | 4            |
| Titan        | 0,275%             | 3            |
| Unknown      | rest, about 1%     | 11           |

We observe that there are 14 big and identifiable miners. The other
participants' hash rates sum up to about 1%. In our simplified scenario
we combine the unknown participants into a single node. Hence we end up
with `n = 15`.

1092 blocks have been mined over the past 7 days, implying an average
block interval of about 554 seconds. Individual block intervals are
exponentially distributed. To meet the average block interval, we scale
the distribution by factor 554 and let `mining_delay()` do the sampling.
`select_miner()` returns a random node according to the hash-rate
distribution in the table.

Regarding the communication, me make a another simplification. We assume
that all nodes are connected to each other and that blocks propagate in
6 seconds with a uniformly distributed jitter of ± 2 seconds.

We further assume that all nodes are honest and that [the specification
of Nakamoto consensus]({{< protocol "nakamoto" >}}) is provided in the
Python module `nakamoto`.

```python
import nakamoto  # protocol specification
from numpy import random


def mining_delay():
    return random.exponential(scale=554)


def select_miner():
    hash_rates = [0.24817, 0.20147, ..., 0.00275, 0.01]  # from table
    return random.choice(range(n), p=hash_rates)


def neighbours(i):
    return [x for x in range(n) if x != i]


def message_delay(src, dst):
    return 6 + random.uniform(low=-2, high=2)


def roots():
    return nakamoto.roots()


def validity(block):
    return nakamoto.validity(block)


def node(i):
    return (nakamoto.init, nakamoto.update, nakamoto.mining)
```

## Concurrent Programming

We describe the simulator as a concurrent program.
The program does not use parallelism---all computations happen
sequentially in a single thread.
We introduce two primitives to delay function evaluation.

- `delay(n, fun, *args)` schedules the evaluation of `fun(*args)` in `n`
  seconds. While waiting, other computations may happen concurrently.
- `delay_until(prop, fun, *args)` schedules the evaluation of
`fun(*args)` as soon as `prop()` returns `True`. While `prop()` returns
`False` other computations may take place concurrently.

Consider the following example program which prints the letters *a* to
*f* in order. We invite the curious reader to take a look at the
[complete program](../concurrent_programming_asyncio.py), including
an implementation of the two primitives `delay` and `delay_until` using
Python's `asyncio`.

{{< code-figure >}}

```python
def f(x):
    f.count += 1
    t = time.time() - f.start
    print(f"at second {t:.0f}: {x}")


f.start = time.time()
f.count = 0

f("a")
delay(2, f, "d")
delay(0, f, "c")
delay(1, delay, 2, f, "f")
delay_until(lambda: f.count >= 4, f, "e")
f("b")

## expected output:
# at second 0: a
# at second 0: b
# at second 0: c
# at second 2: d
# at second 2: e
# at second 3: f
```

Example program demonstrating the two scheduling primitives `delay` and
`delay_until`.
{{< /code-figure >}}

## Initialization

- Initialize empty Block DAG.
- Add `roots()` blocks to the DAG. Make visible to all nodes.
- Initialize node IDs 1 ... `n`.
- Initialize node states with `node(i)["init"](roots)`.

In background run proof-of-work loop.

- wait `mining_delay()` seconds
- obtain node id from `i = select_miner()`
- obtain block draft from `node(i)["mining"]()`
- convert block draft into block, set pow property, check validity
- if valid, deliver block to node `i` with source `"mining"`.
- else, delete block
- re-enter the loop.

Delivery. Arguments `block`, `i`, `source`.

- ensure that block is delivered at most once and in dag-imposed order
  - wait until all parents are visible
  - if block not yet visible continue, else abort delivery
- make block visible to node `i`
- recall old state `old_state = state[i]`
- obtain update `upd = node(i)["update"](old_state, block, source)`
- store new state `state[i] = upd.state`
- handle communication. Node returned request to share messages in
`upd.share`. For each of these messages do `send(m)`.
- handle block appends w/o proof-of-work. Node returned request to
append blocks in `upd.append`. For each of these blocks do `append(m,
i)`.

Communication. Function `send(block, src)`.

- Get neighbours of node `src`.
- For each neighbor `dst`, concurrently do
  - wait for `message_delay(src, dst)` seconds
  - do `deliver(block, dst, "network"`

Block appends w/o proof-of-work. Function `append(block_draft, src)`.

- Convert block draft into global block. Reuse existing blocks if
  possible.
- Set pow property to false
- check block validity
- if valid, do `deliver(src, block, "append")`

```python
dag = DAG()
blocks = [dag.add(b) for b in roots()]
state = [None for _ in range(n)]
for b in blocks:
    for i in range(n):
        dag.make_visible(b, i)
for i in range(n):
    init, _update, _mining = node(i)
    with dag.partial_visibility(i):
        state[i] = init(blocks)

# proof-of-work loop
while true:
    wait_seconds(mining_delay())
    i = select_miner()
    _init, _update, mining = node(i)
    with dag.partial_visibility(i):
        draft = mining()
    block = dag.add(draft)
    block.pow = True
    if validity(block):
        deliver(block, i, "proof-of-work")
    else:
        dag.remove(block)


def deliver(block, i, event):
    # deliver block in DAG-order
    wait_until(
        lambda: all([dag.is_visible(b, i) for b in block.parents()])
    )

    # deliver block once
    if dag.is_visible(block, i):
        return
    dag.make_visible(block, i)

    # simulate node-action
    _init, update, _mining = node(i)
    with dag.partial_visibility(i):
        upd = update(state[i], block, event)

    # handle state update
    state[i] = upd.state

    # handle communication
    for m in upd.share:
        for dst in neighbours(node):
            after_seconds(
                message_delay(node, dst),
                lambda: deliver(dst, m, "network"),
            )

    # handle non-proof-of-work appends
    for draft in upd.append:
        block = dag.add(draft)
        block.pow = False
        if validity(block):
            deliver(block, node, "append")
```

## Discrete event simulation

TODO. Speeding things up with DES.

## Brainstorm

To be removed.

State:

- Global DAG.
- State for each node.
- Block-visibility for each node.

Time:

(implementation detail, nice to know but maybe not fully describe this
here)

- Discrete event simulation
- Time-ordered queue of future events
- Infinite loop consuming & handling first event in the queue
- Event handler might queue future events
- Time between events is skipped
- Single non-blocking process

Appends:

Communication:

Proof-of-work:
