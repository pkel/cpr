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
over an existing network. The protocol specifies how the nodes behave,
that is, how they update their state and what messages they send. It is
useful to think of each node as one physical machine.

On the conceptual layer we have to deal with the nodes' operators. In
some settings---let's say for a distributed database deployed within a
corporation---it's sufficient to model a single operator who deploys all
nodes. In such a setting, one may assume that all nodes follow the
protocol specification. Sure, a node may stop being responsive due to
network congestion, hardware problems, or power outages, but it will not
arbitrarily deviate from the protocol. In distributed systems
literature, this is often called the fail-stop model or model of benign
failures.

For proof-of-work protocols the fail-stop model is not sufficient.
Anybody can add and remove nodes as they want. Consequently, we have to
assume that some nodes deviate from the protocol---usually in order to
maximize the operator's own benefits. Usually it's quite hard to pin
down what deviations are possible or make sense. In order to be on the
safe side, we have to consider arbitrary deviations. Highlighting the
difference to benign failures, arbitrary deviations are often called
Byzantine faults. The discipline around building protocols that work in
in the presence of Byzantine faults is called Byzantine Fault Tolerance
(BFT).

When we say a node is honest or benign, we mean that it follows the
protocol as specified. Nodes that deviate from the protocol are called
malicious nodes. Attackers are operators who deploy malicious nodes.
Defenders are operators who deploy honest nodes.

It is quite obvious that any distributed system stops working as
indented when there are too many malicious nodes. In the extreme case,
all nodes deviate from the protocol and there is nothing to defend. So,
distributed system security always starts with assumptions which limit the
attacker. In traditional distributed systems one would limit the number
of malicious nodes. In proof-of-work systems we limit the hash-rate of
the attacker.

{{< alert icon="👉" text="No security without assumptions!" />}}

When strategic deviations from the protocol are plausible, strategic
manipulation of the underlying network is plausible as well. Some
attackers might have sufficient resources to slow down or even prevent
communication between the defenders. Again, a super strong network-level
attacker can bring down any distributed system. In the most extreme case
the honest nodes cannot communicate at all and hence the distributed
system does not really exist.

Choosing the right assumptions is difficult. Make them too weak and no
useful property can be achieved. Make them too strong and they become
unrealistic. Smart attackers will violate our assumptions whenever
possible.

We postpone making concrete assumptions to the analysis phase of
individual protocols. At this point, we make only minimal assumptions
about the execution model. We assume there are nodes but not how many.
We assume that proof-of-work slows down all nodes, but we do not yet
limit the attacker's hash-rate. We assume that all nodes can send and
receive messages, but we do not yet limit the propagation delays.

## Blobs, Hashes, Blockchain

In practice, nodes are computers. They manipulate binary data locally
and send binary data over a network for communication. The central data
structure of proof-of-work protocols is the hash-linked list. Blobs of
data can refer to other blobs by including a hash of the referenced
blob. The hash-function is practically collision-free, which makes the
list effectively append-only. It's always possible to append a blob to
another, thereby extending (or forking) the list. But it is never
possible to edit referenced blobs. Any change to a blob changes its
hash. Any blob referring to the old version of the changed blob
still includes the old hash and thus refers to the unmodified blob.

In theory, we do not want to deal with binary data and thus introduce an
abstraction for blobs and hash-linking. We reuse the prevalent
terminology. Blobs are now blocks. Each block can have an arbitrary
number of parent blocks (including none), modelling what we called
hash-links above. If block $a$ is a parent of block $b$, then block $b$
is a child of block $a$. Blocks that can be reached with the parent
relationship from block $a$ are called ancestors of $a$. Blocks that can
be reached with the child relationship from block $a$ are called
descendants of $a$. Blocks can store arbitrary data in named fields.
Blocks, their parents, and their fields are persistent. That is,
changing a block creates a copy of the block. Freshly copied blocks do
not have any descendants.

Blocks and the parent relationship form a directed acyclic graph which
we call block DAG. Within the DAG, blocks without parents are
called roots. Blocks without children are called leaves.
Looking at the DAG from roots to children, a parent block clearly comes
before its children, but children of the same parents (siblings) have
the same rank. We say the DAG imposes a partial order on the blocks.

Each block also defines a blockchain, namely the block itself and all
its ancestors. Blockchains have exactly one leave, which we sometimes
refer to as tip of the chain.

## Visibility and Communication

In practice, verifying hash-links requires full knowledge of the
referenced blob. Without knowing the blob, its hash cannot be computed.
Without knowing the hash, the blob cannot be referred to. It is
impossible to distinguish between a link that was intentionally
broken---let's say by replacing the hash with a random number---and a
link to an existing but locally unavailable blob.

For the virtual environment, we introduce the concept of local block
visibility. Each block can be either visible to a node or not. Blocks
that are locally visible to a node will stay locally visible to this
node forever. If a block is locally visible, then all its ancestors are
also locally visible. This restriction does not apply to block children
where we local visibility might be partial.

In other words, we assume that nodes have partial knowledge about the
block DAG. A node's knowledge grows over time as it learns about new
blocks in the order imposed by the DAG.

Nodes can create blocks. For this, all parents must be locally visible.
Freshly created blocks are invisible to all nodes but their creator.
Nodes can decide to share blocks. Depending on the communication
assumptions (made elsewhere), the virtual environment makes visible
shared blocks, including all their ancestors, to the other nodes.
Typically, blocks which have never been shared, and whose descendants
have not been shared, will not be visible to any nodes but its creator.
We say these blocks are withheld by their creator. Usually, honest nodes
do not withhold blocks but malicious nodes do.

We model the determinism of hash-linked lists by allowing nodes to
re-create blocks. If two nodes create two identical blocks---that is,
blocks with the same parents, the same fields, and the same field
contents---then only one block is created and both nodes obtain
visibility of the same block.

## Proof-of-Work

In practice, proof-of-work is about finding blobs that have a small
hash. Typically, the protocol sets a difficulty target or number of
leading zeros the hash must have. Good hash-functions ensure that
finding such blobs is computationally expensive. The only viable solving
algorithm is repeated trial and error: modify the blob, calculate the
hash, count the leading zeroes, repeat while the difficulty target is
not met.

We avoid these expensive computations in our virtual environment.
Instead, each block gets an additional property, whether it has a
proof-of-work or not, which can be set only by the environment, not by
the nodes themselves. Blocks with a proof-of-work are unique, that means
they cannot be re-created by appending a block with the same parents and
fields.

Then, the virtual environment chooses which nodes succeed when at
proof-of-work. Whenever a node succeeds, the environment obtains from
the node a block proposal (parents, fields, and field contents), creates
the corresponding block, sets the proof-of-work property to true, and
then makes visible the new block to the selected node. We now refer to
the selected node as the miner of the new block.

Exact assumptions about the timing and selection of nodes are made
elsewhere. Typically, the time between consecutive mining events is
exponentially distributed with rate $\lambda$. The expected puzzle
solving time is $\lambda^{-1}$. Nodes are selected at random with
weighted probabilities. While $\lambda$ models the overall hash-rate in
the network, the weights model the distribution of hash-rate among the
participants. The attacker's hash-rate, that is, the sum of the
malicious nodes' weights, is at most $\alpha$.
