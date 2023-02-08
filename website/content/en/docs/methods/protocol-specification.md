---
title: "Protocol Specification"
description: |
  How to read protocol specifications.
lead: |
  How to read protocol specifications.
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
attacks or rule them out. Both analysts and engineers want to interpret
the specification as intended by the designer. Practically minded
attackers will attack an implementation, not the specification.

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
three, `init`, `update` and `mining`, specify the node. (Go back to [the
previous section](../virtual-environment) again, if you are confused by
the term *<<node>>*).

The protocol specification might list additional functions for internal
use by other functions or to specify difficulty adjustment and reward
schemes. For now, we will focus on the core functionality and the five
functions mentioned above.

## Blockchain

[A-priori, blocks form a directed acyclic graph
(DAG)](../virtual-environment#blobs-hashes-blockchain). Each block has
an arbitrary number of parent blocks and can store arbitrary data.
Protocol designers can impose additional structure by specifying a
restrictive `validity` function.
This function takes a block as argument. It may return `True`
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
exactly one parent, that each block has a proof-of-work, and that a
block's height is the parent's height plus one. The first rule restricts
the DAG to be a tree.
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

[Nakamoto consensus]({{< protocol "nakamoto" >}}) has a single root
block, often called *<<genesis>>*. It does not have any parent blocks
and its height is zero.
{{< /code-figure >}}

## Node

The blockchain, as defined by `roots` and `validity`, is a global
data-structure. But nodes act on local knowledge. They might [see only a
subset](../virtual-environment#visibility-and-communication) of the
blockchain depending on what information the other nodes share, when
they share it, and how messages propagate through the network.

Protocol designers specify the behaviour of honest nodes through three
functions `init`, `update`, and `mining`.

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
their preferred tip of the chain. Initially they prefer the genesis
block.
{{< /code-figure  >}}

`update` informs the node about new blocks. It takes as argument the
node's old state, the new block, and a string indicating the source of
the block. It returns a new state, a list of blocks to share, and a list
of blocks to append without proof-of-work. The specification consumer
must ensure that

* `update` is called on all blocks as they become locally visible,
* the new block (second argument) and all its ancestors are locally visible,
* the returned state will be given as first argument to the next update,
* the to-be-shared blocks are validated and sent to the other nodes,
* the to-be-appended blocks are validated, appended to the global block
  DAG, and made visible locally,
* the third argument is `"mining"` if the new block was mined
  locally,
* the third argument is `"append"` if the new block was appended locally
  without proof-of-work, and
* the third argument is `"network"` if the new block was received from the
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

`mining` defines what the node appends if successful at proof-of-work.
The function takes as argument the current state of a node and returns
the block that the node wants to mine on. The engineer who implements
the specification and the protocol analyst interpret this function
differently.

The engineer will implement a proof-of-work loop that each node runs
locally in the background.  For each iteration of the loop, the node
takes the latest block proposal from the `mining` function, slightly
permutes it, calculates its hash, and checks whether the hash meets the
mining difficulty target. If yes, the block now has a proof-of-work and
becomes valid. The node handles the new block according to the
specification's `update` function.

The [analyst avoids doing the actual
proof-of-work](../virtual-environment#proof-of-work) and just assumes
that random nodes succeed with mining at random times. Only when a node
succeeds, the analyst obtains a block from the `mining` function, sets
its `has_pow()` property to true, and updates the node with the newly
appended block.

The protocol designer is indifferent to this distinction. She assumes
that

* `b.has_pow()` is true if and only if `b` was appended through
proof-of-work, and
* when a node learns about a successful proof-of-work with
`update(old_state, new_block, "mining")` it holds that
`new_block == mining(old_state)`.

{{< code-figure >}}

```python
def mining(b: Block):
    return Block(height=b.height + 1, parents=[b], miner=my_id)
```

In [Nakamoto consensus]({{< protocol "nakamoto" >}}) all blocks require
a proof-of-work. Nodes mine to append new blocks to their preferred tip
of the chain. Miners include their ID to facilitate disbursement of
mining rewards.
{{< /code-figure >}}

## Difficulty Adjustment

Proof-of-work intentionally slows down the creation of new blocks and
hence the growth of the blockchain. Typical proof-of-work protocols try
to maintain a constant growth rate. For example, Bitcoin tries to
achieve 6 blocks per hour, by updating the proof-of-work puzzle
difficulty every 2016 blocks. We use the term difficulty adjustment (DA)
to refer to the general problem of controlling chain growth.
Concrete DA mechanisms are called difficulty adjustment algorithms
(DAA).

Designing DAAs is quite challenging. A good DAA reacts quickly and
correctly to changes in the networks *true* hash-rate. Unfortunately,
the true hash-rate cannot be observed. In practice, nodes add a
timestamp to each block. The DAA then estimates the current hash-rate
from past timestamps. From there it deterministically adjusts the
difficulty for future blocks. In this scheme, there are many sources of
error. First, mining is a stochastic process. Puzzle solving times are
exponentially distributed, implying high variance. Observations of
puzzle solving time are noisy. Second, nodes are distributed around the
planet and might disagree about the current time. Third, nodes might try
to intentionally confuse the DAA by lying about the current time. Forth,
timestamps in orphaned blocks cannot be used. In short, difficulty
adjustment is a complex control problem. It even has its own line of
research.

Protocol designers use the `progress` function to specify what kind of
growth the DAA should control for. The function maps a given block (tip
of blockchain) to a scalar value. The engineer implementing the protocol
will contribute a DAA that controls for constant progress per time. The
analyst may just assume constant progress per time.

{{< code-figure >}}

```python
def progress(b: Block):
    return b.height
```

[Nakamoto consensus](../nakamoto) uses the block height as progress. The
DAA tries to achieve constant progress per time. Bitcoin targets 6
blocks per hour.
{{< /code-figure >}}

## Rewards

Deployed blockchain protocols usually implement some sort of virtual
currency. Funds are associated with cryptographic key-pairs. Moving
funds from one key-pair to another requires access to the source
key-pair's private part. This introduces a new kind of system
participant, namely the holders of the private keys and hence
*<<owners>>* of the associated funds. We call them crypto-currency
users.

In principle, crypto-currency users and node operators can be disjunct.
Users may operate a node but they do not have to. But in practice
blockchain protocols motivate participation as operator by handing out
crypto-currency denoted rewards. Hence node operators usually are
crypto-currency users.

As protocol designers, we want our protocol to accommodate as many
applications as possible. We thus avoid imposing requirements on the
crypto-currency. We just assume that each node has access to a unique
identifier `my_id` that can receive rewards. In practice, `my_id` would
be the account or wallet address of the node's operator. The
specification itself however does not have any notion of crypto-currency
address or wallet.

We specify incentive mechanisms using four functions `local_tip`, `global_tip`,
`history` and `reward`.

* `local_tip` takes a node's state as argument and returns its preferred
tip of the chain.
* `global_tip` selects the impartially best tip among all nodes' preferred
tips.
* `history` calculates the linear history of the best tip.
* `reward` maps a block (in the linear history) to reward assignments.
* Each reward assignment assigns a scalar reward to a node id.

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

In [Nakamoto consensus]({{< protocol "nakamoto" >}}) the node's state is
its preferred tip of the chain. The best tip is the longest chain. The
history is the blockchain itself and the rewards are constant.
{{< /code-figure >}}

Splitting reward calculation into these four functions enables us to
calculate different useful metrics like

* subjective rewards for each node,
* objective rewards in case of disagreement,
* individual rewards for each block, and
* accumulated historic rewards per node for any tip of the chain.

Note that this reward specification can model situations where miners
get assigned rewards for blocks that are not part of the linear history.
This happens for example in [tree-structured voting]({{< protocol
"parallel-tree"
>}}).
