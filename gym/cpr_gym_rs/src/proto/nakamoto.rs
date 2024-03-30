use crate::generic::intf::{BlockDAG, Protocol};

struct Nakamoto {}

struct Data {
    height: u32,
}

impl<DAG, Block, Miner> Protocol<DAG, Block, Miner, Data> for Nakamoto
where
    DAG: BlockDAG<Block, Miner, Data>,
    Block: Copy,
    Miner: Copy,
{
    fn init(&self) -> Data {
        Data { height: 0 }
    }

    fn mining(&self, d: DAG, ep: Block) -> (Vec<Block>, Data) {
        let parents = vec![ep];
        let data = Data {
            height: d.data(ep).height + 1,
        };
        (parents, data)
    }

    fn update(&self, d: DAG, ep: Block, b: Block) -> Block {
        if d.data(b).height > d.data(ep).height {
            b
        } else {
            ep
        }
    }

    fn tip(&self, _d: DAG, ep: Block) -> Block {
        ep
    }

    fn pred(&self, d: DAG, b: Block) -> Option<Block> {
        let p = d.parents(b);
        if p.len() > 0 {
            Some(d.parents(b)[0])
        } else {
            None
        }
    }

    fn progress(&self, _d: DAG, _b: Block) -> f32 {
        1.
    }

    fn reward(&self, d: DAG, b: Block) -> Vec<(Miner, f32)> {
        vec![(d.miner(b), 1.)]
    }
}
