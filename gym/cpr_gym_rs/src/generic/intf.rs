// Protocol Specification Interface

pub trait BlockDAG<Block, Miner, Data> {
    // BlockDAG navigation: backward
    fn parents(&self, b: Block) -> Vec<Block>;
    // What blocks does `b` refer to?
    // In practice, `b` would include the hashes of the blocks referred to.
    // Sometimes we ambiguously say *confirms* instead of *refers to*. Other times, we say
    // *confirms* when we mean to include transitive references.

    // BlockDAG navigation: forward
    fn children(&self, b: Block) -> Vec<Block>;
    // What blocks refer to `b`?

    fn miner(&self, b: Block) -> Miner;
    // Who mined block `b`?
    // This will be used to allocate rewards.

    fn data(&self, b: Block) -> &Data;
    // Extract protocol-dependent block data.
}

pub trait Protocol<DAG, Block, Miner, Data>
where
    DAG: BlockDAG<Block, Miner, Data>,
{
    // genesis block
    fn init(&self) -> Data;

    // honest behaviour: mining
    fn mining(&self, d: DAG, ep: Block) -> (Vec<Block>, Data);
    // Given entrypoint `ep`, how do honest nodes extend the chain?
    // The returned vec feeds into the parents of the newly mined block.

    // honest behaviour: preference update
    fn update(&self, d: DAG, ep: Block, b: Block) -> Block;
    // Given entrypoint `ep` and novel block `b`, how do honest nodes update their entrypoint?

    // linear history: tip of chain
    fn tip(&self, d: DAG, ep: Block) -> Block;
    // Given entrypoint `ep`, what is considered the latest block in the linear chain of blocks?

    // linear history: walking backwards
    fn pred(&self, d: DAG, b: Block) -> Option<Block>;
    // Given block `b`, what's the parent of `b` in the linear chain of blocks?

    // difficulty adjustment
    fn progress(&self, d: DAG, b: Block) -> f32;
    // Given block `b`, what's the mining contribution since `pred(b)`?
    // DAA's goal tries to maintain a constant mining contribution rate per time.
    // Block `b` is guaranteed to be part of a linear history of some tip.

    // incentive scheme
    fn reward(&self, d: DAG, b: Block) -> Vec<(Miner, f32)>;
    // Given block `b`, what rewards where allocated since `pred(b)`?
    // Block `b` is guaranteed to be part of a linear history of some tip.
}
