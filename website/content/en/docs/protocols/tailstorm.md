---
title: "Tailstorm"
description: |
  Tailstorm consensus protocol. WIP.
lead: |
  Tailstorm consensus protocol. WIP.
draft: false
images: []
menu:
  docs:
    parent: "protocols"
weight: 313
toc: true
mermaid: true
---

## Intuition

To be written. I'll do the easier protocols first.

## Example

{{< mermaid-figure >}}
```mermaid
graph RL
  classDef orphan opacity:0.5,fill:#eee
  v0([1]) --> s0[h]
  v2([2]) --> v1([1]) --> s0
  s1[h+1] --> v0 & v2
  s2[h+2] --> v5([3]) --> v4([2]) --> v3([1]) --> s1
  s3[h+3] --> v8([1]) &  v7([1]) & v6([1]) --> s2

  v5a([2]):::orphan --> v3
  s2a[h+2]:::orphan --> v4 & v5a
  v6a([1]):::orphan --> s2a

  linkStyle 15,16,17,18 opacity:0.5
```
Tailstorm with three sub-blocks per summary. Square boxes represent
summary blocks and round boxes represent sub-blocks. Only sub-blocks
require a proof-of-work. Summary blocks are labelled with their height,
sub-blocks with their depth. The gray blocks are orphaned.
{{< /mermaid-figure >}}

## Specification

### Parameters

`k`: number of sub-blocks per summary-block

### Blockchain

```python
def roots():
    return [Block(height=0, miner=None, kind="summary")]


def last_summary(b: Block):
    b = b.parents()[0]
    while b.kind != "summary":
        b = b.parents()[0]
    return b


def confirmed_sub_blocks(b: Block):
    set = {}
    for p in b.parents():
        if p.kind == "sub-block":
            set |= {p}
            set |= confirmed_sub_blocks(p)
    return set


def confirming_sub_blocks(b: Block):
    set = {}
    for c in b.children():
        if c.kind == "sub-block":
            set |= {c}
            set |= confirming_sub_blocks(c)
    return set


def validity(b: Block):
    if b.kind == "summary":
        s = last_summary(b)
        assert len(confirmed_sub_blocks(b)) == k
        assert b.height == s.height + 1
        assert b.depth == 0
        for x in confirmed_sub_blocks(b):
            assert last_summary(x) == s
    elif b.kind == "sub-block":
        parents = b.parents()
        assert len(parents) == 1
        assert b.has_pow()
        assert b.depth == parents[0].depth + 1
    return False
```


### Node

```python
def init(roots: [Block]):
    return roots[0]


def preference(old: Block, new: Block):
    assert new.kind == "summary"
    if new.height > old.height:
        return new
    if new.height < old.height:
        return old
    n_old = len(confirming_sub_blocks(old))
    n_new = len(confirming_sub_blocks(new))
    if n_new > n_old:
        return new
    if n_new < n_old:
        return old
    r_old = my_reward(old)
    r_new = my_reward(new)
    if r_new > n_old:
        return new
    return old


def summarize(b: Block):
    assert b.kind == "block"
    if len(confirming_sub_blocks(b)) < k:
        return []  # summary infeasible
    else:
        # Select leaves in sub-block tree such that the tree includes
        # k sub-blocks and own-reward is maximized.
        leaves = ...
        return [
            Block(kind="summary", height=b.height + 1, parents=leaves)
        ]


def update(old: Block, new: Block, event: string):
    if new.kind == "summary":
        return Update(state=preference(old, new))
    else:  # new.kind == "sub-block"
        b = last_summary(new)
        return Update(
            state=preference(old, b),
            share=[new] if event == "mining" else [],
            append=attempt_summary(b),
        )


def mining(b: Block):
    best = b
    for sb in confirming_sub_blocks(b):
        if sb.depth > p.depth or (
            sb.depth == p.depth and p.miner == my_id
        ):
            best = sb
    return Block(
        kind="sub-block",
        depth=best.depth + 1,
        parents=[best],
        miner=my_id,
    )
```

### Rewards

#### Constant reward

```python
def constant_reward(b: Block):
    if b.kind == "summary":
        return [Reward(x.miner, 1) for x in confirming_sub_blocks(b)]
```

{{< mermaid-figure >}}
```mermaid
graph RL
  classDef orphan opacity:0.5,fill:#eee
  v0([1]) --> s0[n/a]
  v2([1]) --> v1([1]) --> s0
  s1[n/a] --> v0 & v2
  s2[n/a] --> v5([1]) --> v4([1]) --> v3([1]) --> s1
  s3[n/a] --> v8([1]) &  v7([1]) & v6([1]) --> s2

  v5a([n/a]):::orphan --> v3
  s2a[n/a]:::orphan --> v4 & v5a
  v6a([n/a]):::orphan --> s2a

  linkStyle 15,16,17,18 opacity:0.5
```
Constant reward applied to the example blockchain shown above. Only the
miners of sub-blocks get assigned rewards.
{{< /mermaid-figure >}}


#### Discount reward

```python
def discount_reward(b: Block):
    if b.kind == "summary":
        d = max([x.depth for x in b.parents()])
        return [Reward(x.miner, d / k) for x in confirmed_sub_blocks(b)]
```

{{< mermaid-figure >}}
```mermaid
graph RL
  classDef orphan opacity:0.5,fill:#eee
  v0([2/3]) --> s0[n/a]
  v2([2/3]) --> v1([2/3]) --> s0
  s1[n/a] --> v0 & v2
  s2[n/a] --> v5([3/3]) --> v4([3/3]) --> v3([3/3]) --> s1
  s3[n/a] --> v8([1/3]) &  v7([1/3]) & v6([1/3]) --> s2

  v5a([n/a]):::orphan --> v3
  s2a[n/a]:::orphan --> v4 & v5a
  v6a([n/a]):::orphan --> s2a

  linkStyle 15,16,17,18 opacity:0.5
```
Discount reward applied to the example blockchain shown above. Observe
how the reward scheme punishes non-linearity.
{{< /mermaid-figure >}}

<!--

## Attacks

### Selfish Mining

Description.

### SSZ attack space

Description.

## CPR API

How to simulate, attack, learn.

-->