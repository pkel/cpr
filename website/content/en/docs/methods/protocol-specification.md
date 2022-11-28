---
title: "Protocol Specification"
description: |
  How to read protocol specifications.
lead: |
  How to read protocol specifications.
date: 2020-10-06T08:49:31+00:00
lastmod: 2020-10-06T08:49:31+00:00
draft: false
images: []
menu:
  docs:
    parent: "method"
weight: 220
toc: true
---

Protocol designers write protocol specifications for consumption by
analysts and engineers. The engineer reads the specification and
implements it. The analysts reads the specification and tries to find
attack or rule out their possibility. Both analysts and engineers want
to interpret the specification as intended by the designer. Practically
minded attackers will attack the implementation, not the specification.

Depending on the context, we take the role of the designer or the
analysts. We never take the role of the engineer or the practically
minded attacker. When we discuss attacks, we generally assume that
engineers implement the protocol as specified.

We specify protocols in pseudo-code. That is, we write protocols as
programs but these programs are meant for human consumption. We prefer
readability over elegance and elegance over efficiency. We use Python's
syntax and core functionality hoping that this makes our protocol
specifications accessible to a wide audience.

A protocol specification consists of at least five functions: `roots`,
`validity`, `init`, `update` and `mining`. The first two, `roots` and
`validity` specify the structure of the protocol's blockchain. The other
three, `init`, `update` and `mining`, specify the node. (Read [the
previous section](../virtual-environment) again, if you are confused by
the term <<node>>).

The protocol specification might list additional functions for internal
use by other functions or to specify difficulty adjustment and reward
schemes. For now, we will focus on the core functionality and the five
functions mentioned above.

## Blockchain

Protocols specify and use a global, append-only data-structure.
A-priori, blocks form a directed acyclic graph (DAG). Each block has an
arbitrary number of parent blocks and can store arbitrary data. The
`validity` function then restricts what blocks can be appended and
thereby imposes a certain structure on the DAG.

The `validity` function takes a block as argument. It may return `True`
or `False`, or it may fail. The specification consumer must ensure that
all appended blocks are valid, that is, `validity` returns `True`.

{{< code-figure >}}
```python
def validity(b: Block):
    assert len(b.parents()) == 1
    assert b.has_pow()
    assert b.height == b.parents()[0].height + 1
    return True
```
The `validity` function of [Nakamoto
consensus]({{< protocol "nakamoto" >}}) specifies that each block has
exactly one parent, that each block must have a proof-of-work, and that
a block's height is the parent's height plus one. The first rule
restricts the DAG to be a tree.
{{< /code-figure >}}

Protocol designers may specify an initial set of blocks which do not
have to be valid. This is done in the specification's `roots` function.
It takes no argument and returns a list of blocks. The specification
consumer must ensure that these blocks are part of the DAG when protocol
executions begins.

{{< code-figure >}}
```python
def roots():
    return [Block(height=0, miner=None)]
```
[Nakamoto consensus]({{< protocol "nakamoto" >}}) has single root block,
often called <<genesis>> block. It does not have any parent blocks. It's
height is zero.
{{< /code-figure >}}

## Node

The blockchain, as defined by `roots` and `validity`, is a global
data-structure. But nodes, [that is](../virtual-environment),
network-connected protocol participants, act on local knowledge. They
might see only a subset of the blockchain depending on what information
the other nodes share, when they share it, and how messages propagate
through the network.

Nodes are specified with three functions `init`, `update`, and `mining`.

`init` takes a list of blocks as argument and returns the node's initial
state. The specification consumer must ensure that the list of blocks
given to `init` is the list of blocks defined by the global `roots`
function.

{{< code-figure >}}
```python
def init(roots: [Block]):
    return roots[0]
```
[Nakamoto consensus]({{< protocol "nakamoto">}}) nodes use as state
their preferred tip of the chain. Initially they prefer the <<genesis>>
block.
{{< /code-figure  >}}

`update` informs the node about new blocks. It takes as argument the
node's old state, the new block, and a string indicating how the new
block became visible locally. It returns a new state, a list of blocks
to share, and a list of blocks to append without proof-of-work. The
protocol designer must ensure that

* `update` is called on all blocks as they become locally visible,
* the new block given as argument and all its ancestors are locally visible,
* the returned state will be given as first argument to the next update,
* the to-be-shared blocks are validated and sent to the other nodes,
* the to-be-appended blocks are validated, appended to the global DAG
data-structure, and made visible locally,
* the `event` argument is `"mining"` if the new block was mined
locally,
* the `event` argument is `"append"` if the new block was appended locally
without proof-of-work, and
* the `event` argument is `"network"` if the new block was received from the
network.

{{< code-figure >}}
```python
def update(old: Block, new: Block, event: string):
    if event == "mining":
        return Update(state=new, share=[new])  # implicitly: append = []
    elif new.height > old.height:
        return Update(state=new)  # implicitly: share & append = []
    else:
        return Update(state=old)
```
In [Nakamoto consensus]({{< protocol "nakamoto" >}}) nodes always prefer
the longest chain of blocks, measured by block-height. Newly mined
blocks are shared immediately with the other participants.
{{< /code-figure >}}

Speaking of proof-of-work, we come to the `mining` function of the node
specification. It takes as argument the current state of a node and
returns the block that the node wants to mine on. The engineer who
implements the specification and the protocol analyst interpret this
function differently.

The engineer will implement a proof-of-work loop that each node runs
locally in the background.  For each iteration of the loop, the node
takes the latest block proposal from the `mining` function, slightly
permutes it, calculates its hash, and checks whether the hash meets the
mining difficulty target. If yes, the block now has a proof-of-work and
becomes valid. The node handles the new block according to the
specification's `update` function.

The analyst avoids doing the actual proof-of-work and just assumes that
random nodes succeed with mining at random times. Only when a node
succeed, the analyst obtains a block proposal from the `mining`
function, sets its `has_pow()` property to true, and updates the node
with the newly appended block.

The protocol designer is indifferent to this distinction. The designer
assumes that
- proof-of-work is computationally expensive,
- `b.has_pow()` is true if and only if `b` was appended through
proof-of-work, and
- when a node learns about a successful proof-of-work with
`update(old_state, new_block, event = "mining")` it holds that
`new_block == mining(old_state)`.

{{< code-figure >}}
```python
def mining(b: Block):
    return Block(height=b.height + 1, parents=[b], miner=Env.my_id)
```
In [Nakamoto consensus]({{< protocol "nakamoto" >}}) all blocks require
a proof-of-work. Nodes mine to append new blocks to their preferred tip
of the chain. Miners include their id to facilitate disbursement of
mining rewards.
{{< /code-figure >}}

## Rewards

Deployed blockchain protocols usually implement some sort of virtual
currency. Funds are associated with cryptographic key-pairs. Moving
funds from one key-pair to another requires access to the source
key-pair's private part. This introduces a new kind of system
participants, namely the holders of the private keys and hence holders
of the associated funds. We call them crypto-currency users. In
principle, crypto-currency users and node operators decoupled concepts.
Users may operate a node but they do not have to. But in practice,
blockchain protocols motivate participation as operator by handing out
crypto-currency denoted rewards. Hence node operators usually are
crypto-currency users.

As consensus protocol designers, we want our protocol to accommodate as
many applications as possible. We avoid imposing requirements on the
crypto-currency. We just assume that each node has access to a unique
identifier `my_id` that can receive rewards. In practice, `my_id` would
be the account or wallet address of the node's operator. The
specification however does not have any notion of crypto-currency
address or wallet.

We specify reward calculation using four functions `local_tip`, `global_tip`,
`history` and `reward`.
- `local_tip` takes a node's state as argument and returns its preferred
tip of the chain.
- `global_tip` selects the impartially best tip among all nodes' preferred
tips.
- `history` calculates the linear history of the best tip.
- `reward` maps a block (in the linear history) to reward assignments.
- A reward assignment assigns a scalar reward to a node id.

{{< code-figure >}}
```python
def local_tip(b: Block):
    return b


def global_tip(l: [Block]):
    b = l[0]
    for i in range(1, len(l)):
        if l[i].height > b.height:
            b = l[i]
    return b


def history(b: Block):
    h = [b]
    p = b.parents()
    while p != []:
        h.append(p[0])
        p = p[0].parents()
    return h


def reward(b: Block):
    return [Reward(b.miner, 1)]
```
[Nakamoto consensus]({{< protocol "nakamoto" >}}) is quite boring when it
comes to rewards: the node's state is the preferred tip of the chain.
The best tip is the longest chain. The history is the blockchain itself,
and the rewards are constant. Other protocols, e.g.
[tree-structured voting]({{< protocol "parallel-tree" >}}), use more
elaborate schemes.
{{< /code-figure >}}

Splitting reward calculation into these four functions enables us to
calculate different useful metrics like

- subjective rewards for each node,
- objective rewards, even in case of disagreement,
- individual rewards for each block, and
- accumulated rewards per node for any block in the DAG.

The reward API can be used to model situations where miners get assigned
rewards for blocks that are not part of the linear history. This happens
for example in [tree-structured voting]({{< protocol "parallel-tree"
>}}).
