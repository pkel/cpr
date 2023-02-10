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

## Block DAG and Visibility

As [discussed before]({{< method "virtual-environment" >}}) the
simulator will maintain a DAG of all blocks mined or appended by any
nodes. Individual nodes however have only a partial (but growing) view
on this DAG. In our program, we set local views as follows.

```python
dag = DAG()
...
# in this context dag represents the global view on _all_ blocks
with dag.set_view(i):
    ...
    # in this context visibility of parents and children is restricted
    # according to the local view of node i
...
# in this context dag represents the global view on _all_ blocks
```

Local views are read-only. The node functions `init`, `mining`, and
`update` cannot append blocks to the DAG. Instead, they return requests
for appending a block to the simulator. We will describe below, how the
simulator handles these requests.

The local view makes the nodes' id accessible to the node by setting
`my_id = i` within the context.

## Initialization

We start each simulation with the initialization of the block DAG,
the protocol's `root` blocks, and the nodes' state.

```python
# initialize empty block DAG
dag = DAG()

# obtain drafts for the protocol's root blocks, convert them into
# actual blocks, and make them visible to all nodes.
root_blocks = [dag.add(b) for b in roots()]
for b in roots_blocks:
    for i in range(n):
        dag.make_visible(b, i)

# nodes' state
state = [None for _ in range(n)]
for i in range(n):
    init, _update, _mining = node(i)
    with dag.set_view(i):
        state[i] = init(blocks)
```

## Proof-of-Work

After initialization, the simulator starts a concurrent loop which
models proof-of-work and mining. The loop repeatedly samples and waits
for a mining delay, selects a random node as successful miner, obtains a
block draft from this miner, converts the draft into a block, checks
block validity, and informs the miner about the freshly mined block.

```python
def proof_of_work():
    # select random miner
    i = select_miner()
    # obtain block draft from miner
    _init, _update, mining = node(i)
    with dag.set_view(i):
        draft = mining()
    # convert draft to block w/ proof-of-work
    block = dag.add(draft)
    block.has_pow = True
    # enforce validity of all appended blocks
    if validity(block):
        # inform miner about freshly mined block
        deliver(block, i, "proof-of-work")
    else:
        dag.remove(block)
    # continue proof-of-work loop after random delay
    delay(mining_delay(), proof_of_work)


# enter proof-of-work loop
delay(mining_delay(), proof_of_work)
```

## Block Delivery

Block delivery is what we call informing a node about a new block.
From the perspective of a node, new blocks might be freshly mined
locally, just appended locally, or received from the network.

We handle these deliveries in the `deliver` function. It makes the block
visible to the node, then it obtains and handles a state update from the
node's `update` function.

```python
def deliver(block, i, event):
    # update local visibility
    dag.make_visible(block, i)

    # simulate node i's action
    _init, update, _mining = node(i)
    with dag.set_view(i):
        ret = update(state[i], block, event)

    # store updated state
    state[i] = ret.state

    # handle communication
    for msg in ret.share:
        broadcast(i, msg)

    # handle appends w/o proof-of-work
    for draft in ret.append:
        append(i, draft)
```

The [protocol specification]({{< method "protocol-specification" >}})
assumes that blocks are delivered in order, that is, that the parents of
a delivered block have been delivered before. For our previous use of
`deliver` in the proof-of-work loop, this invariant is trivially true.
The following wrapper ensures the invariant for messages received from
the network.

```python
def deliver_in_order(block, i, event):
    def deliver_once(block, i, event):
        if not dag.is_visible(block, i):
            deliver(block, i, event)

    def all_parents_visible():
        for parent in block.parents():
            if not dag.is_visible(parent, i):
                return False
        return True

    delay_until(parents_visible, deliver_once, block, i, event)
```

## Communication

Function `broadcast(block, src)`.

- Get neighbours of node `src`.
- For each neighbor `dst`, concurrently do
  - wait for `message_delay(src, dst)` seconds
  - do `deliver(block, dst, "network"`

```python
def broadcast(src, block):
    for dst in neighbours(src):
        t = message_delay(src, dst)
        delay(t, deliver_in_order, dst, block, "network")
```

## Non-PoW Blocks

Block appends w/o proof-of-work. Function `append(block_draft, src)`.

- Convert block draft into global block. Reuse existing blocks if
  possible.
- Set pow property to false
- check block validity
- if valid, do `deliver(src, block, "append")`

```python
def append(i, draft):
    block = dag.add(draft)
    block.pow = False
    if validity(block):
        deliver(block, node, "append")
```

## Discrete Event Simulation

[Above](#concurrent-programming) we suggest to use concurrent
programming to implement the scheduling primitives `delay` and
`delay_until`. If implemented naively, the simulator would spend most
the time waiting for the delays or checking properties becoming true.
A delays of `n` seconds would actually take `n` seconds to
simulate. Simulations would be real-time, but real-time is
inconveniently slow in the proof-of-work blockchain context.

We speed up the simulation by skipping the delayed time instead of
waiting for it to pass. The approach is called [discrete-event
simulation](https://en.wikipedia.org/wiki/Discrete-event_simulation).
The trick is to maintain a time-ordered queue of future events and let
an infinite loop handle the scheduled events one after another until
none are left. In the context of this simulator, future events are
delayed function calls. Handling an event is as simple as evaluating the
call. The simulator functions schedule further delayed calls. The
time-ordering of the queue ensures that the calls happen in the same
order as they would with naive waiting.

```python
from queue import PriorityQueue

time = 0
queue = PriorityQueue()
props = []


def delay(seconds, fun, *args):
    queue.put(time + seconds, fun, args)


def delay_until(prop, fun, *args):
    prop.append(prop, fun, args)


# simulator code goes here
...

while not queue.empty():
    # dequeue and handle next event
    time, fun, args = queue.get()
    fun(*args)
    # re-evaluate delay_until properties
    next_props = []
    for prop, fun, args in props:
        if prop():
            fun(*args)
        else:
            next_props.append(prop, fun, args)
    props = next_props
```

Note that using `delay_until` can cause significant overhead. We
introduced this scheduling primitive to improve readability of the
simulator pseudo-code. We completely avoid this kind of scheduling in
our optimized simulator implementation.
