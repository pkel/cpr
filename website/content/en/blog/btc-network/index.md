---
title: "Modelling the Bitcoin Network"
description: |
  (draft). This block post does not have enough content yet.
excerpt: |
  (draft). This block post does not have enough content yet.
date: 2023-02-13
draft: true
weight: 50
images: ["taylor-vick-M5tzZtFCOfs-unsplash.jpg"]
categories: ["Constructions"]
tags: ["bitcoin", "network", "assumptions"]
contributors: ["Patrik Keller"]
pinned: false
homepage: false
---

**TL;DR:** (draft). This block post does not have enough content yet.

In this blog posts I'll shortly present a simple but realistic network
scenario that models Bitcoin as it is deployed today. We'll have a look
at the Bitcoin mining statistics and model our
[virtual network]({{< method "simulator" >}}#inputs) accordingly.

We obtain an estimate about the hash-rate distribution from
[blockchain.com](https://www.blockchain.com/explorer/charts/pools).
The following table shows mining statistics for the last 7 days before
December 14th 2022.

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

There are 14 big and identifiable miners. The other participants' hash
rates sum up to about 1%. We simplify the network by combining the
unknown participants into a single node. Hence we end up with `n = 15`.

1092 blocks have been mined over the past 7 days, implying an average
block interval of about 554 seconds. The underlying proof-of-work
puzzle can only be solved by repeated guessing. Thus individual block
intervals are exponentially distributed. We define `mining_delay()` to
meet these observations. `select_miner()` returns a random node
according to the hash-rate distribution in the table.

Regarding the communication, me make a another simplification. We assume
that all nodes are connected to each other and that blocks propagate in
6 seconds with a uniformly distributed jitter of ± 2 seconds.

We further assume that all nodes are honest. We load [the specification
of Nakamoto consensus]({{< protocol "nakamoto" >}}) from the
module `nakamoto`.

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
