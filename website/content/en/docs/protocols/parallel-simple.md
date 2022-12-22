---
title: "Parallel PoW (simplified)"
description: |
  Simplified implementation of parallel proof-of-work.
lead: |
  Simplified implementation of parallel proof-of-work.
draft: false
images: []
menu:
  docs:
    parent: "protocols"
weight: 310
toc: true
mermaid: true
---

## Intuition

[Nakamoto consensus](../nakamoto) enforces a linear chain of blocks. Of
any two blocks with equal height, only one will be retained. The others
get orphaned. Network-level attackers can strategically time message
delivery such that the defenders produce more orphans. When the attacker
engages with the defenders in a race for the longest chain, orphans
effectively slow down the defender and the attacker becomes stronger.
Parallel proof-of-work minitages this problem by introducing parallelism
where Nakamoto's blockchain is strictly linear.

Parallel proof-of-work distinguishes between blocks and votes. The
blocks still form a linear chain, but the votes can be mined in
parallel. Votes for the same parent block are compatible, even if their
miners cannot communicate. Orphans can only happen at the transition
between blocks.

In this simple version of parallel proof-of-work, both blocks and votes
require a proof-of-work. Appending a new block requires $k - 1$ votes
for the previous block. Together with the proof-of-work required for the
block itself, this makes $k$ proofs-of-work per block.

The [original version of parallel proof-of-work](../parallel-aft22)---as
presented by Keller and Böhme at AFT '22---is a bit more complicated.
Make sure you understand this simple version before you explore the
AFT '22 version.

## Example

{{< mermaid-figure >}}

```mermaid
graph RL
  b0([h  ])
  b1([h+1]) --> v0([ ]) & v01([ ]) & v02([ ]) --> b0
  b2([h+2]) --> v3([ ]) & v04([ ]) & v05([ ]) --> b1
  b3([h+3]) --> v6([ ]) & v07([ ]) & v08([ ]) --> b2
  b4([h+3]) --> v9([ ]) & v10([ ]) & v11([ ]) --> b3
```

Parallel proof-of-work with three votes per block. Idealized case w/o
orphans. Blocks are labelled with their height. Votes have no label.
Both blocks and votes require a proof-of-work.
{{< /mermaid-figure >}}

{{< mermaid-figure >}}

```mermaid
graph RL
  b0([h  ])
  b1([h+1]) --> v00([ ]) & v01([ ]) & v02([ ]) --> b0
  b2([h+2]) --> v03([ ]) & v04([ ]) & v05([ ]) --> b1
  b3([h+3]) --> v06([ ]) & v07([ ]) & v08([ ]) --> b2
  b4([h+3]) --> v09([ ]) & v10([ ]) & v11([ ]) --> b3

  classDef orphan opacity:0.5,fill:#eee
  o1([ ]):::orphan --> b0
  o2([ ]):::orphan --> b1
  o3([h+2]):::orphan --> o2 & v05 & v04
  o4([h+3]):::orphan --> v06 & v07 & v08
  o5([ ]):::orphan --> o4
  linkStyle 24,25,26,27,28,29,30,31,32 opacity:0.5
```

The same parallel proof-of-work blockchain with potential orphans in gray.
{{< /mermaid-figure >}}

## Specification

Have a look at [the methodology page for protocol specification]({{< method
"protocol-specification" >}}) to learn how to read this.

### Parameters

`k`: number of proofs-of-work per block (or number of votes per
block plus one)

### Blockchain

```python
def roots():
    return [Block(height=0, miner=None, kind="block")]


def parent_block(b: Block):
    if b.kind == "block":
        return b.parents()[0].parents()[0]
    else:
        return b.parents()[0]


def validity(b: Block):
    assert b.has_pow()
    if b.kind == "block":
        p = parent_block(b)
        assert len(b.parents()) == k - 1
        assert b.height == p.height + 1
        for x in b.parents():
            assert x.parents()[0] == p
    elif b.kind == "vote":
        assert len(b.parents()) == 1
    return False
```

### Node

```python
def init(roots: [Block]):
    return roots[0]


def preference(old: Block, new: Block):
    assert new.kind == "block"
    if new.height > old.height:
        return new
    if new.height < old.height:
        return old
    n_old = len(old.children())
    n_new = len(new.children())
    if n_new > n_old:
        return new
    return old


def update(old: Block, new: Block, event: string):
    if new.kind == "block":
        consider = new
    else:
        consider = parent_block(new)
    return Update(
        state=preference(old, consider),
        share=[new] if event == "mining" else [],
    )


def mining(b: Block):
    assert b.kind == "block"
    if len(b.children()) < k - 1:
        return Block(kind="vote", parents=[b], miner=my_id)
    else:
        # select k - 1 votes; own votes first, then old before new
        votes = ...
        return Block(
            kind="block",
            height=b.height + 1,
            parents=votes,
            miner=my_id,
        )
```

### Difficulty Adjustment

```python
def progress(b: Block):
    if b.kind == "block":
        return b.height * k
    else:
        p = parent_block(b)
        return p.height * k + 1
```

### Rewards

```python
def local_tip(b: Block):
    return b


def global_tip(l: [Block]):
    b = l[0]
    for i in range(1, len(l)):
        b = preference(b, l[i])
    return b


def history(b: Block):
    h = [b]
    p = b.parents()
    while p != []:
        b = p[0]
        if b.kind == "block":
            h.append(b)
        p = b.parents()
    return h


def reward(b: Block):
    assert b.kind == "block"
    return [Reward(x.miner, 1) for x in [b] + b.parents()]
```

{{< mermaid-figure >}}

```mermaid
graph RL
  b0([1])
  b1([1]) --> v00([1]) & v01([1]) & v02([1]) --> b0
  b2([1]) --> v03([1]) & v04([1]) & v05([1]) --> b1
  b3([1]) --> v06([1]) & v07([1]) & v08([1]) --> b2
  b4([1]) --> v09([1]) & v10([1]) & v11([1]) --> b3

  classDef orphan opacity:0.5,fill:#eee
  o1([n/a]):::orphan --> b0
  o2([n/a]):::orphan --> b1
  o3([n/a]):::orphan --> o2 & v05 & v04
  o4([n/a]):::orphan --> v06 & v07 & v08
  o5([n/a]):::orphan --> o4
  linkStyle 24,25,26,27,28,29,30,31,32 opacity:0.5
```

Blockchain depicted above with reward scheme applied to the longest chain.
Each proof-of-work rewards its miner with one unit of reward. Naturally,
orphans are not rewarded.
{{< /mermaid-figure >}}

<!--

## Attacks

### Selfish Mining

Description.

### SSZ-like attack space

Description.

## CPR API

How to simulate, attack, learn.
-->

## Literature

This protocol is a simplified version of the protocol $\mathcal B_k$
presented by Keller and Böhme.

- Keller and Böhme. Parallel Proof-of-Work with Concrete Bounds.
AFT '22. [[preprint]](https://arxiv.org/abs/2204.00034)
