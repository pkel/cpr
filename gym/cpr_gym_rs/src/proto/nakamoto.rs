use crate::generic::intf::{BlockDAG, Protocol};

struct Nakamoto {}

struct Data {
    height: u32,
}

impl Protocol for Nakamoto {
    type Data = Data;

    fn init(&self) -> Data {
        Data { height: 0 }
    }

    fn mining<D: BlockDAG<Data>>(&self, d: D, ep: D::Block) -> (Vec<D::Block>, Data) {
        let parents = vec![ep];
        let data = Data {
            height: d.data(ep).height + 1,
        };
        (parents, data)
    }

    fn update<D: BlockDAG<Data>>(&self, d: D, ep: D::Block, b: D::Block) -> D::Block {
        if d.data(b).height > d.data(ep).height {
            b
        } else {
            ep
        }
    }

    fn tip<D: BlockDAG<Data>>(&self, d: D, ep: D::Block) -> D::Block {
        ep
    }

    fn pred<D: BlockDAG<Data>>(&self, d: D, b: D::Block) -> Option<D::Block> {
        let p = d.parents(b);
        if p.len() > 0 {
            Some(d.parents(b)[0])
        } else {
            None
        }
    }

    fn progress<D: BlockDAG<Data>>(&self, d: D, b: D::Block) -> f32 {
        1.
    }

    fn reward<D: BlockDAG<Data>>(&self, d: D, b: D::Block) -> Vec<(D::Miner, f32)> {
        vec![(d.miner(b), 1.)]
    }
}
