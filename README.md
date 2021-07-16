# ToDo List

## Withholding Inefficiency

Investigate why

```
CPR_ACTIVATIONS=1000000 dune exec experiments/withholding.exe -- data/withholding.tsv
```

takes ages. Maybe record time spent per run in `csv_runner`.

## Simulate other networks

Get network spec (compute/latencies) from other papers

1. Loiseau. http://arxiv.org/abs/2106.02970
2. Tschorsch. https://eprint.iacr.org/2021/884

## Implement other protocols

1. Patrik's B_k with PoW hash-ordering based leader selection.
2. BU's protocol after George gave me the specifics.

## Implement more attacks

1. SelfishAdvanced on B_k_lessleader: Attacker could mines more votes on
the next private block, while the defender cannot yet produce an block
themselves.

2. SelfishSimple/Advanced on unmodified B_k.

3. SelfishSimple/Advanced on George's protocol.

George calls SelfishSimple "proof packing" and SelfishAdvanced
"long-range".

## Reward after Difficulty Adjustment

We assume constant exchange rate.

Attacker wants to maximize individual absolute reward per time. Bitcoin
has a constant reward per confirmed block. Bitcoin's DAA tries to keep
the number of confirmed blocks per time constant. Thereby, the DAA also
maintains constant overall absolute reward per time. Selfish mining
literature maximizes for short term expected relative reward and argues
that this also maximizes individual absolute reward per time in the long
run (after difficulty adjustment).

This reasoning relies on the fact that the DAA optimizes for constant
overall reward per time. George's DAA presumably optimizes for constant
block rate. Some of Georges reward schemes do not have constant reward
per block. Hence short term relative reward may not be a valid proxy for
long term individual absolute reward.

In a latent network, DAA creates a feedback loop: Structure of the DAG
feeds into DAA, controls difficulty, affects structure of the DAG.
Simulating selfish mining in latent networks requires modelling of the
DAA.

In a non-latent network, we might get away without implementing the DAA.
