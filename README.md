# TODO

1. Implement protocol "george" that resembles a modification of
Bₖ/lessleader where votes reference the most recent vote. I.e. vote for
the longest chain of votes, produce block when you have k votes.

2. Get network spec (compute/latencies) from Patrick Loiseau's
paper/code. Run simulator on our three protocols. Count votes/blocks.
Apply his Gini efficiency metric.

3. Implement reward schemes. Repeat step 2.

# Experiment

I want to tabulate f(block time, protocol) where
 - block time ∈ 30s ... 10min
 - protocol ∈ Nakamoto, B₁...B₁₀₀ + George's with various reward variants
 - f ∈
    + orphaned pow / activations
    + orphaned pow / blocks
    + efficiency = confirmed pow / activations
      - of weakest miner
      - of weakest miner / of strongest miner
      - gini index
    + efficiency = relative reward / relative compute
      - of weakest miner
      - of weakest miner / of strongest miner
      - gini index

1. Run simulations for all combinations of block time and protocol.
2. Write CSV
3. Create tables in R/Python
