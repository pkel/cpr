---
title: "Generalizing Selfish Mining's Gamma"
description: |
  We discuss a popular network assumption made in the Selfish Mining
  literature against Nakamoto consensus. We derive an equivalent but
  more general network assumption which can be used with any protocol
  and other attacks than Selfish Mining.
excerpt: |
  We discuss a popular network assumption made in the Selfish Mining
  literature against Nakamoto consensus. We derive an equivalent but
  more general network assumption which can be used with any protocol
  and other attacks than Selfish Mining.
date: 2022-12-15
draft: false
weight: 50
images: []
categories: ["Constructions"]
tags: ["related work", "selfish mining", "assumptions"]
contributors: ["Patrik Keller"]
pinned: false
homepage: false
---

{{< alert icon="👉" text="WIP. You are looking at an unfinished post." />}}

## Missing Pieces

This post has a couple of dependencies which are not yet written.
Not sure whether I want to release stuff in didactic order or as they
flow through my keyboard. If you read this, I apologize for not being a
good teacher.

* The example in [the simulator docs section]({{< method "simulator">}})
should probably come as an example blog post. I could calculate its
$\gamma$ here or over there.
* I'll have to present Eyal's original Selfish Mining attack and
Sapirshtein's attack space somewhere.
* In a post similar to this one, I could explain how to generalize
relative reward with reward per progress.

## Selfish Mining's Gamma Parameter

In their original work on Selfish Mining against [Nakamoto
consensus]({{< protocol "nakamoto" >}}), Eyal and Sirer (2014) make the
following network assumption. What they refer to as *<<selfish pool>>*
would be called attacker in our notation.

> If the [selfish] pool has a private branch of length 1 and the others
mine one block, the pool publishes its branch immediately, which results
in two public branches of length 1. Miners in the selfish pool all mine
on the pool’s branch, because a subsequent block discovery on this
branch will yield a reward for the pool. The honest miners, following
the standard Bitcoin protocol implementation, mine on the branch they
heard of first. We denote by $\gamma$ the ratio of honest miners that
choose to mine on the pool’s block, and the other $(1 - \gamma)$ of the
non-pool miners mine on the other branch.

In their follow-up work, Sapirshtein et al. (2016) summarise as follows.

> $\gamma$ is a parameter representing the communication capabilities of
the attacker—the fraction of nodes to which it manages to send blocks
first in case of a block race.

I like this approach because it reduces a wide variety of possible
network conditions into a single, explainable parameter. The big
drawback is the close coupling of the definition with the [attacked
protocol]({{< protocol "nakamoto" >}}) and the [corresponding attack
space](#missing-pieces).

## CPR's Generic Network

[CPR's simulator]({{< method "simulator" >}}) has a generic interface to
support a wide variety of network assumptions. Nodes have outgoing
connections. If a node shares a block, it will be disseminated across
these connections. Message delays can be controlled individually per
sending node, outgoing connection, and message.

{{< img-figure src="net4.png" alt="Fully-connected network with 4 nodes" >}}
CPR network example. Circles represent nodes, arrows represent outgoing
connections. The four nodes are fully connected; message forwarding is
not necessary.
{{< /img-figure >}}

In the remainder of this post, I will describe how to model CPR networks
that exhibit a given $\gamma.$ We can then use these networks to
reproduce the existing Selfish Mining results against [Nakamoto
consensus]({{< protocol "nakamoto" >}}) within CPR. Even better, we can
launch other attacks against any specified protocol while keeping the
network assumptions compatible with the Selfish Mining literature.

## Reordering Messages

In the Selfish Mining literature, $\gamma$ represents the share of
defenders (by hash-rate) who adopt the attacker's block in case of a
block race. The term block race refers to a situation where one of the
honest nodes mines a new block $b_0$ while the attacker already has
mined---but kept private---a block $b_1$ of the same height. If the
attacker decides to release $b_1$ at the very same time, the defenders'
reactions depend on individual message propagation delays and hence the
network assumptions. Defender's will proceed mining on the block they
received first. Attackers with network-level capabilities can delay
propagation of $b_0$ and hence make the defenders prefer the attacker's
block $b_1$. The prevalent assumption is that, after each block race
$\gamma$ of the defender's hash-rate is used to mine on $b_1$, the rest
on $b_0$.

The block race situation uses what BFT researches would call message
reordering. The honest miner sends $b_0$ before the attacker sends
$b_1$, but (some of) the other defender nodes receive $b_1$ before $b_0$.
The $\gamma$ parameter abstractly defines how often and to which
defenders this reordering succeeds.

Based on the more general notion of message reordering, we can derive a
CPR-network exhibiting a given $\gamma.$

## Constructing a Network

We start by imposing a couple of restrictions on the design space.
First, the network should consist of $n \geq 3$ nodes. One attacker and
at least two defenders. We need at least two defenders---one sends a
message before the attacker does, the other receives the two messages,
potentially in reversed order. Second, all defenders have the same
hash-rate. Third, the nodes are fully-connected, that is, each node has
outgoing connections to all other nodes. Forth, the attacker receives
all messages immediately. Fifth, message delays between defenders are
constant but small, let's say $\varepsilon$. Sixth, all outgoing
connections from the attacker to defender have the same properties.

{{< img-figure src="net4+links.png" alt="Restricted network" >}}
Example network with $n = 4$ nodes. Node 0 is the only malicious node.
Nodes 1 to 3 are defender nodes. Propagation delay from defender to
attacker (green) is zero; from defender to defender (blue) it's
$\varepsilon$. We'll choose the propagation delay from attacker to
defender (red) to meet a certain $\gamma$.
{{< /img-figure >}}

With these restrictions in place, we have only one dimension left. How
do we have to delay messages from attacker to defender, such that
$\gamma$ of the block races end in favor of the attacker?

There are one attacker node and $n - 1$ defender nodes. One of the
defenders sends a message---in Selfish Mining that would be a freshly
mined block. The other $n - 2$ defenders receive the message after
$\varepsilon$ time. The attacker receives it immediately, sends another
message---usually a competing block---to the $n-2$ receiving defenders,
and hopes that it gets there first, that is, faster than $\varepsilon$
time.

Let's rename the nodes such that $0$ is the attacker and node $1$ is the
sending defender node. Nodes $2$ to $n$ are about to receive the
messages sent by nodes $0$ and $1$. Then, we introduce a couple of
random variables. $D_i$ is the random message delay from the attacker
$0$ to defender $i$. We assume that $D_i$ is uniformly distributed
between $0$ and $d$. $X_i$ is one if node $i$ receives the attackers
message first and zero otherwise. $X$ describes how many defenders
receive the attacker's message first. All of this applied in reverse
order yields

$$
E[X]
  = \sum_{n=2}^{n-1} E[X_i]
  = \sum_{n=2}^{n-1} P[D_i < \varepsilon]
  = \sum_{n=2}^{n-1} \frac{\varepsilon}{d}
  = (n - 2) \cdot \frac{\varepsilon}{d}
  \quad.
$$

Now, recall that $\gamma$ represents the number of nodes who mine on the
attackers block after the block race. In other words, we want $\gamma$
of the defenders to receive the attacker's message first. Mathematically
speaking,

$$
\frac{E[X]}{n - 1} \stackrel{!}{=} \gamma \quad.
$$

Substituting $E[X]$ from above and resolving for $d$ yields

$$
d = \frac{n - 2}{n - 1} \cdot \frac{\varepsilon}{\gamma} \quad.
$$

Following this reasoning backwards implies that if the message delays
from attacker to defender are independently drawn from the continuous
uniform distribution on the interval $[0, \frac{n - 2}{n - 1} \cdot
\frac{\varepsilon}{\gamma}]$, then the network is compatible with the
$\gamma$-assumption in Selfish Mining.

## Limiting Gamma

It is common practice to evaluate Selfish Mining for $\gamma = 1$.
This is not possible with our approach. But $\gamma = 1$ is also
not possible in practice.

The reason is that in any block race one of the honest nodes has mined
the block which the attacker tries to match. This honest node will never
accept the attacker's competing block. Instead, it will continue mining
on the honest block.

The same is true for message reordering. In order to craft a competing
message, the attacker has to know the message. While the sending
defender has not yet sent the message, the attacker cannot react to it.
Hence, the sender of the message cannot receive the attacker's message
first.

In our network with $n-1$ defenders this manifest as follows. After any
block race, $\frac{1}{n-1}$ of the defenders' hash-rate will be used to mine
on the honest block. Consequently, $\gamma$ must be lower or equal $1 -
\frac{1}{n-1} = \frac{n-2}{n-1}$. In reverse, in order to emulate a
given $\gamma$ we have to simulate at least $n \geq \frac{1}{1 -
\gamma}+ 1$ nodes. Setting $\gamma = 1$ implies $n = \infty$ and makes
simulations unfeasible.

Astute readers will now (at the latest) start questioning the formulas
in the previous section. Why does the limitation presented here not show
up above? It's because one of the steps requires qualification. We used
that $D_i$ is uniformly distributed on the interval from $0$ to $d$.
Then we said that $P[D_i < \varepsilon] = \frac{\varepsilon}{d}$. That's
true if $d \geq \varepsilon$ but otherwise the probability is $1$.
As a result, $E[X] \leq n-2$ and $\gamma \leq \frac{n-2}{n-1}$.

In the real world, most of the hash-rate is concentrated around a view
nodes. In December 2022, the two biggest Bitcoin mining pools have 26%
and 19% of the overall hash-rate. Assume the bigger one turns malicious
and starts selfish mining. That leaves 74% of the hash rate to the
defender, of which about 26% belong to the second-biggest miner
(the 19% one). In 26% of the block races, the second-biggest miner will
be the sender of the block which the attacker tries to outrun. So in
26% of the cases 26% of the defender hash-rate will not mine on the
attackers block. This alone makes $\gamma > 0.94$ unrealistic.

## Choosing the Network Size

To be written.

* We need at least 3 nodes.
* We need at least $\frac{1}{1 - \gamma} + 1$ nodes.
* Selfish Mining hits target $\gamma$ exactly for each block race.
* Our network achieves $\gamma$ in expectation.
* More nodes $\rightarrow$ less variance.

## Literature

* Ittay Eyal and Emin G. Sirer. Majority is not enough: Bitcoin mining
is vulnerable. FC '14; CACM '18.
[[preprint]](https://arxiv.org/abs/1311.0243)
[[publisher]](https://link.springer.com/chapter/10.1007/978-3-662-45472-5_28)
[[publisher]](https://dl.acm.org/doi/abs/10.1145/3212998)
* Ayelet Sapirshtein, Yonathan Sompolinsky, and Aviv Zohar. Optimal
Selfish Mining strategies in Bitcoin. FC '16.
[[preprint]](https://arxiv.org/abs/1507.06183)
[[publisher]](https://link.springer.com/chapter/10.1007/978-3-662-54970-4_30)
