// Protocol Specification Interface

pub trait BlockDAG {
    type Block: Copy;
    type Miner: Copy;
    type BlockIter: Iterator<Item = Self::Block>;

    // BlockDAG navigation: backward
    fn parents(&self, b: Self::Block) -> Self::BlockIter;
    // What blocks does `b` refer to?
    // In practice, `b` would include the hashes of the blocks referred to.
    // Sometimes we ambiguously say *confirms* instead of *refers to*. Other times, we say
    // *confirms* when we mean to include transitive references.

    // BlockDAG navigation: forward
    fn children(&self, b: Self::Block) -> Self::BlockIter;
    // What blocks refer to `b`?

    fn miner(&self, b: Self::Block) -> Self::Miner;
    // Who mined block `b`?
    // This will be used to allocate rewards.

    fn height(&self, b: Self::Block) -> u32;
    // TODO, move this into crate::proto::nakamoto
}

pub trait Protocol {
    type BlockIter<D: BlockDAG> : Iterator<Item = D::Block>;

    // honest behaviour: mining
    fn mining<D: BlockDAG>(&self, d: D, ep: D::Block) -> Self::BlockIter<D>;
    // Given entrypoint `ep`, how do honest nodes extend the chain?
    // The returned iterator feeds into the parents of the newly mined block.

    // // honest behaviour: preference update
    fn update<D: BlockDAG>(&self, d: D, ep: D::Block, b: D::Block) -> D::Block;
    // // Given entrypoint `ep` and novel block `b`, how do honest nodes update their entrypoint?

    // // linear history: tip of chain
    fn tip<D: BlockDAG>(&self, d: D, ep: D::Block) -> D::Block;
    // // Given entrypoint `ep`, what is considered the latest block in the linear chain of blocks?

    // // linear history: walking backwards
    fn pred<D: BlockDAG>(&self, d: D, b: D::Block) -> D::Block;
    // // Given block `b`, what's the parent of `b` in the linear chain of blocks?

    // // difficulty adjustment
    fn progress<D: BlockDAG>(&self, d: D, b: D::Block) -> f32;
    // // Given block `b`, what's the mining contribution since `pred(b)`?
    // // DAA's goal tries to maintain a constant mining contribution rate per time.
    // // Block `b` is guaranteed to be part of a linear history of some tip.

    // // incentive scheme
    type RewardAlloc<D: BlockDAG>: Iterator<Item = (D::Miner, f32)>;
    fn reward<D: BlockDAG>(&self, d: D, b: D::Block) -> Self::RewardAlloc<D>;
    // // Given block `b`, what rewards where allocated since `pred(b)`?
    // // Block `b` is guaranteed to be part of a linear history of some tip.
}
