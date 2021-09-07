# Consensus Protocol Research

We evaluate a number of proof-of-work consensus protocols by discrete
event network simulation. We implement simulation, protocols, and
specific attack in OCaml (continuation of [my own work][hotpow]). We
enable the search for optimal attack strategies by providing a Python OpenAI
Gym interface to the simulator (inspired by [Hou et al.][squirrl]).

[hotpow]: https://arxiv.org/abs/1907.13531
[squirrl]: https://www.ndss-symposium.org/ndss-paper/squirrl-automating-attack-analysis-on-blockchain-incentive-mechanisms-with-deep-reinforcement-learning/

## Discrete Event Simulation

The simulator maintains state for simulated nodes separately in a single
process. The simulator emulates time based on a priority queue of
events. There are two types of events:

- `Activate` models the events that a simulated node finds a
  proof-of-work puzzle solution.

- `Deliver` models delivery of a message to one of the simulated nodes.

Virtually, each event takes place at a discrete point in time. The
simulator's main loop takes the first event from the queue and invokes
the associated event handler. The simulator's event handlers schedule
future events by inserting them into the queue.

## Broadcast

Simulated nodes can use a broadcast primitive to share information over
a structured network. For each invocation, the simulator schedules
message delivery for all neighboring nodes in the event queue. The
simulator can inject independent random propagation delays between messages
broadcast and delivery per receiving node. The simulator supports two
methods of message dissemination.

- `Simple` assumes a fully connected network topology. Nodes do not
forward received messages. Propagation delays follow the random delays
configured on individual links.

- `Flooding` assumes partially connected network topology. Nodes forward
received messaged to their peers. Individual message propagation delays
depend on the realizations of the links of all possible paths.

Currently, we only use simple dissemination on homogeneous networks (all
honest scenario) and networks with zero delay (attack scenario).

**To do:** Get and use network specifications (compute and latencies)
from other papers ([Loiseau][loiseau], [Tschorsch][tschorsch]).

[loiseau]: http://arxiv.org/abs/2106.02970
[tschorsch]: https://eprint.iacr.org/2021/884

## Proof-of-Work

The simulator avoids unnecessary computation using a stochastic clock.
During initialization, the simulator enqueues one `Activate` event the
event queue. For each `Activate` event taken from the front of the
queue, the simulator schedules another `Activate` event with a
random, exponentially distributed delay.

Simulated nodes have access to primitives that enable the verification
of simulated proof-of-work.

## DAG and Partial Visibility

Simulated nodes can share information (see Broadcast).
Each shareable datum can have hash-links to other shareable data. The
simulator models the hash-links in a single directed acyclic graph
(DAG). Each simulated network node operates on a local view on the DAG.
The local view restricts visibility of the DAG vertices according to
delivery status of network messages.

**Note:** We try to consistently use the terms "node" for participants
of the network/protocol and "vertex" for elements of the DAG.

## Protocols

A protocol imposes restrictions on the DAG (e.g., roots, structure, and
necessity of proof-of-work solutions) and describes the honest
participant. Protocols live in `ocaml/protocols/*.ml`, one protocol per
file.

- `Nakamoto` is the longest chain consensus as first deployed with Bitcoin.

- `B_k` is a variant of [HotPoW][hotpow] without the three-staged
commit. It's currently under review. I've implemented both HotPoW and Bₖ
for a similar but less modular [simulation][ppow] before. In a nutshell,
participating nodes mine votes that reference the last seen block. As
soon as there are k votes, the participant with the smallest vote (by
proof-of-work hash) proposes a new block.

- `B_k_lessleader` is a variant of `B_k` that does not elect leaders
based on the ordering of the hash-values of the involved proof-of-work
votes. Assume any participant of `B_k` could propose a block. Then
everybody would propose an own block. Disambiguation would happen with the
next dissemination of the next vote(s). `B_k_lessleader` does this but
integrates the next vote into the block proposal to avoid redundant
podcasts. In [HotPoW][hotpow] terminology, `B_k_lessleader` block
proposals require one bound-to-proposal (BTP) puzzle solution in a
addition to k-1 bound-to-identifier (BTI) votes.

- `George` is a variant of `B_k_lessleader` where votes reference back
to the latest known vote instead of the last block. The votes for a
single block form a tree. The tree structure can be used for advanced
incentive schemes (e.g., total block reward may depend on the depth of
the tree of votes).

**Note:** George uses the term block for what I call vote and strong
block for what I call block.

[ppow]: https://github.com/pkel/hotpow/tree/ppow

**To do:** Consider adding Ethereum (Nakamoto + uncle rewards) and Bobtail.

## Private Attack

`ocaml/lib/privateAttack.ml` implements a general withholding attack
where the attacker simulates two honest nodes `private_` and `public`.
Both nodes process message delivery but do not share any information
created by themselves automatically. `private_` handles proof-of-work
activations. `public` can see foreign or released information only. A
policy controls which information is released and whether `private_`
should prefer `public`'s tip of chain as preferred node.

**Note:** It follows naturally that `private_` adopts `public`'s tip of
chain whenever an honest node would do so.

**To do:** Consider modeling attacks where adoption of `public`'s tip of
chain must happen explicitly.

**To do:** Instantiate this attack for `B_k_lessleader` and `George`.

## Visualization

We visualize protocol executions by plotting the resulting DAG
structure. The simulator generates Graphviz files for rendering
with the `dot` command. `make visualize` simulates and renders a number
of configurations defined in `ocaml/experiments/visualize.ml`.

Buggy protocol implementation may yield malformed DAGs and raise
`Malformed_DAG` exceptions. The malformed DAG can be logged to Graphviz
format for further inspection by setting the `CPR_MALFORMED_DAG_TO_FILE`
environment variable.

**To do:** Graphviz is convenient from a programmer's perspective.
Export to and rendering by iGraph would have a few advantages. E.g., we
could plot time on the x-axis and mining node on the y-axis.

## Selfish Mining Policies

We are aware of three attacks, all can be described as policy for the
private attack above.

1. Withhold until anybody can cast the next (strong) block, then prefer
own votes for this block. George calls this proof packing. We call it
selfish-simple here.

2. Withhold until defender can form the next (strong) block, then
overwrite if possible, else adopt. George calls this long-range. We call
is selfish-advanced here.

3. Withhold until defender is about to catch up, then overwrite.

**To do:** Implement and evaluate (3.) for all protocols.

**To do:** Rename selfish-simple and selfish-advanced to selfish-0 and
selfish-1.

## Reinforcement Learning

The attack policies described above might be sub-optimal. We follow [Hou
et al.][squirrl] and expose the private attack as an [OpenAI
gym][openai-gym] environment. This enables easy access from Python-based
policy learning tools to the protocol logic implemented in OCaml.

The OCaml bits of the gym environment live in `ocaml/gym` and are
compiled into a shared object `bridge.so` which can be loaded from
Python. A python package living `python/gym` wraps the exposed OCaml
interface into an OpenAI-compatible `gym.Env` class.

[openai-gym]: https://gym.openai.com/

**To do:** Expose private attack on `Nakamoto`, `B_k_lessleader`, and
`George` to Gym environment.


## Reward after Difficulty Adjustment

We assume constant exchange rates. A selfish miner has fixed expenses
per time and wants to maximize his absolute reward per time.

Bitcoin has a constant reward per confirmed block. Bitcoin's difficulty
adjustment algorithm (DAA) tries to keep the number of confirmed blocks
per time constant. Hence, the DAA also maintains somewhat constant
overall absolute reward per time. Selfish mining literature maximizes
for short term expected relative reward and argues that this also
maximizes the attacker's absolute reward per time in the long run (after
difficulty adjustment).

This reasoning relies on the fact that the DAA optimizes for constant
overall reward per time. George's DAA presumably optimizes for constant
block rate while some of George's reward schemes do not imply a constant
reward per block. Hence, short term relative reward may be invalid as a
proxy for long term individual absolute reward.

In networks w/o propagation delays, we can get away without simulating
the DAA by rescaling the time axis after simulation. In a latent
network, the DAA creates a feedback loop: The structure of the DAG feeds
into DAA, the DAA controls difficulty, the difficulty affects the
structure of the DAG. Evaluation of selfish mining in latent networks
requires modeling of the DAA or simplifying assumptions (e.g. messages
reordering succeeds with fixed probability).

**To do:** Consider simulating DAAs.
