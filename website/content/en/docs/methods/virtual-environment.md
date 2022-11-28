---
title: "Virtual Environment"
description: |
  Execution model for proof-of-work protocols.
lead: |
  Execution model for proof-of-work protocols.
draft: false
images: []
menu:
  docs:
    parent: "method"
weight: 210
toc: true
---

CPR's core component is a network simulation engine for proof-of-work
protocols. The engine takes a protocol specification and a network
topology as input, then simulates the execution of the protocol in the
network over time. In this document we will focus on the theoretical
aspects of this virtual environment. We will describe what we simulate
and why but not how. In other words, we will define a model for virtual
protocol execution.

## Distributed Systems

From a technical perspective, proof-of-work blockchains are distributed
systems. The individual system participants are called nodes. Nodes
maintain local state and communicate with each other through messages
over an existing network. The protocol specifies how nodes behave, that
is, how they update their state and what messages they send. It is
useful to think of each node as one process or machine.

On the conceptual layer we have to deal with the nodes' operators. In
some settings---let's say for a distributed database deployed within a
corporation---it's sufficient to model a single operator who deploys all
nodes. In such a setting, we may assume that all nodes follow the
protocol specification. Sure, a node may stop being responsive due to
network congestion, hardware problems, or power outages, but it will not
arbitrarily deviate from the protocol. This is often called the
fail-stop model or model of benign failures.

For proof-of-work protocols the fail-stop model is not sufficient.
Anybody can add and remove nodes as they want. Consequently, we have to
assume that some nodes deviate from the protocol---usually in order to
maximize the operator's own benefits. Usually it's quite hard to pin
down what deviations are possible or make sense. In order to be on the
safe side, we have to consider arbitrary deviations. Highlighting the
difference to benign failures, arbitrary deviations are often called
Byzantine faults. The discipline around building protocols that work in
in the presence Byzantine faults is called Byzantine Fault Tolerance
(BFT).

When we say a node is honest or benign, we mean that it follows the
protocol as specified. Nodes that do not follow the protocol are called
deviating or malicious nodes. Attackers are operators who deploy
deviating nodes. Defenders are operators who deploy honest nodes.

It is quite obvious that any distributed system stops working as
indented when there are too many malicious nodes. In the extreme case,
all nodes deviate from the protocol and there is nothing to defend. So
distributed system security always starts from assumptions limiting the
attacker. In traditional distributed systems one would limit the number
of malicious nodes. In proof-of-work systems we limit the hash-rate of
the attacker.

When we consider strategic deviations from the protocol, we might as
well consider strategic manipulation of the underlying network. Some
attackers might have sufficient resources to slow down or even prevent
communication between the defenders. Again, a super strong network-level
attacker can bring down any distributed system. In the most extreme case
the honest nodes cannot communicate at all and hence the distributed
system does not really exist.

{{< alert icon="👉" text="No security without assumptions!" />}}

Needless to say, choosing the right assumptions is the hard part. Make
them to weak and no useful property can be achieved. Make them to strong
and they become unrealistic. Smart attackers will violate our
assumptions whenever possible.

We postpone making concrete assumptions to the analysis phase of
individual protocols. Here, we make only minimal assumptions about the
execution model. We assume there are nodes but not how many. We assume
that proof-of-work slows down all nodes, but we do not yet limit the
attacker's hash-rate. We assume that all nodes can send and receive
messages, but we do not yet limit the propagation delays.

## Nodes

There are $n$ nodes. We use $N = \{1,\dots,n\}$ to denote the set of
enumerated nodes.

## Data, Hashes and Storage

To be revised. I'm torn between exposing hash-functions and just saying
there is a "global DAG".

Nodes can manipulate arbitrary data locally. We assume an injective
mapping $\mathcal{H}$ from data to the set of integers. For any datum
$x$, we say that $\mathcal{H}(x)$ is the hash of $x$. A datum may refer
to other data by including the hashes. Nodes can store data
locally. Nodes can retrieve stored data addressed by its hash.

{{< alert icon="👉" text="We assume that $\mathcal{H}$ is a collision-free hash function." />}}

## Communication

To be written...

## Proof-of-Work

To be written...
