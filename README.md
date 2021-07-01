# Loiseau's network as reference

Get network spec (compute/latencies) from Patrick Loiseau's paper/code.

# Experiment all honest network

Configure network with all nodes honest.
Then tabulate f(block time, protocol) where
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
    + gini index of absolute reward - gini index of compute
    + gini index of confirmed pow - gini index of compute

1. Run simulations for all combinations of block time and protocol.
2. Write CSV
3. Create tables in R/Python
