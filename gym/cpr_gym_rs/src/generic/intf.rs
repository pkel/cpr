// Protocol Specification Interface

pub trait BlockDAG<Data> {
    type Block: Copy;
    type Miner: Copy;

    // BlockDAG navigation: backward
    fn parents(&self, b: Self::Block) -> Vec<Self::Block>;
    // What blocks does `b` refer to?
    // In practice, `b` would include the hashes of the blocks referred to.
    // Sometimes we ambiguously say *confirms* instead of *refers to*. Other times, we say
    // *confirms* when we mean to include transitive references.

    // BlockDAG navigation: forward
    fn children(&self, b: Self::Block) -> Vec<Self::Block>;
    // What blocks refer to `b`?

    fn miner(&self, b: Self::Block) -> Self::Miner;
    // Who mined block `b`?
    // This will be used to allocate rewards.

    fn data(&self, b: Self::Block) -> &Data;
    // Extract protocol-dependent block data.
}

pub trait Protocol {
    type Data; // protocol-dependent block data

    // genesis block
    fn init(&self) -> Self::Data;

    // honest behaviour: mining
    fn mining<D: BlockDAG<Self::Data>>(&self, d: D, ep: D::Block) -> (Vec<D::Block>, Self::Data);
    // Given entrypoint `ep`, how do honest nodes extend the chain?
    // The returned vec feeds into the parents of the newly mined block.

    // // honest behaviour: preference update
    fn update<D: BlockDAG<Self::Data>>(&self, d: D, ep: D::Block, b: D::Block) -> D::Block;
    // // Given entrypoint `ep` and novel block `b`, how do honest nodes update their entrypoint?

    // // linear history: tip of chain
    fn tip<D: BlockDAG<Self::Data>>(&self, d: D, ep: D::Block) -> D::Block;
    // // Given entrypoint `ep`, what is considered the latest block in the linear chain of blocks?

    // // linear history: walking backwards
    fn pred<D: BlockDAG<Self::Data>>(&self, d: D, b: D::Block) -> Option<D::Block>;
    // // Given block `b`, what's the parent of `b` in the linear chain of blocks?

    // // difficulty adjustment
    fn progress<D: BlockDAG<Self::Data>>(&self, d: D, b: D::Block) -> f32;
    // // Given block `b`, what's the mining contribution since `pred(b)`?
    // // DAA's goal tries to maintain a constant mining contribution rate per time.
    // // Block `b` is guaranteed to be part of a linear history of some tip.

    // // incentive scheme
    fn reward<D: BlockDAG<Self::Data>>(&self, d: D, b: D::Block) -> Vec<(D::Miner, f32)>;
    // // Given block `b`, what rewards where allocated since `pred(b)`?
    // // Block `b` is guaranteed to be part of a linear history of some tip.
}
