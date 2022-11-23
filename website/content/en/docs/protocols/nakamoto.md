---
title: "Nakamoto"
description: |
  Satoshi Nakamoto's consensus protocol as deployed in Bitcoin.
lead: |
  Satoshi Nakamoto's consensus protocol as deployed in Bitcoin.
date: 2020-10-06T08:49:31+00:00
lastmod: 2020-10-06T08:49:31+00:00
draft: false
images: []
menu:
  docs:
    parent: "protocols"
weight: 301
toc: true
---

<!--

Intro/background/literature.

## Example blockchain

Figure.

-->

## Specification

__DAG Specification__

```python
def roots():
    return [Block(height=0, miner=None)]


def validity(b: Block):
    assert len(b.parents()) == 1
    assert b.has_pow()
    assert b.height == b.parents()[0].height + 1
    return True
```

__Node Specification__

```python
def init(roots: [Block]):
    return roots[0]


def update(old: Block, new: Block, event: string):
    if event == "proof-of-work":
        return Update(state=new, share=[new])
    elif new.height > old.height:
        return Update(state=new)
    else:
        return Update(state=old)


def mining(b: Block):
    return Block(height=b.height + 1, parents=[b], miner=Env.my_id)
```

__Reward Specification__

```python
def reward(b: Block):
    return [Reward(b.miner, 1)]
```

<!--

## Attacks

### Selfish Mining

Description.

### SSZ attack space

Description.

## CPR API

How to simulate, attack, learn.

-->
