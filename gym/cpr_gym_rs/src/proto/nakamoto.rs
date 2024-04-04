use crate::generic::intf;
use crate::generic::intf::BlockDAG;

pub struct Protocol {}

pub struct Data {
    height: u32,
}

impl<Block, Miner> intf::Protocol<Block, Miner, Data> for Protocol
where
    Block: Copy,
    Miner: Copy,
{
    fn init(&self) -> Data {
        Data { height: 0 }
    }

    fn mining<DAG: BlockDAG<Block, Miner, Data>>(&self, d: &DAG, ep: Block) -> (Vec<Block>, Data) {
        let parents = vec![ep];
        let data = Data {
            height: d.data(ep).height + 1,
        };
        (parents, data)
    }

    fn update<DAG: BlockDAG<Block, Miner, Data>>(&self, d: &DAG, ep: Block, b: Block) -> Block {
        if d.data(b).height > d.data(ep).height {
            b
        } else {
            ep
        }
    }

    fn tip<DAG: BlockDAG<Block, Miner, Data>>(&self, _d: &DAG, ep: Block) -> Block {
        ep
    }

    fn pred<DAG: BlockDAG<Block, Miner, Data>>(&self, d: &DAG, b: Block) -> Option<Block> {
        let p = d.parents(b);
        if p.len() > 0 {
            Some(d.parents(b)[0])
        } else {
            None
        }
    }

    fn progress<DAG: BlockDAG<Block, Miner, Data>>(&self, _d: &DAG, _b: Block) -> f32 {
        1.
    }

    fn reward<DAG: BlockDAG<Block, Miner, Data>>(&self, d: &DAG, b: Block) -> Vec<(Miner, f32)> {
        vec![(d.miner(b), 1.)]
    }
}
