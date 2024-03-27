use crate::generic::intf::{BlockDAG, Protocol};

struct Nakamoto {}

struct Data {
    height: u32,
}

use std::iter::{once, Once};

impl Protocol for Nakamoto {
    type Data = Data;
    type BlockIter<D: BlockDAG<Data>> = Once<D::Block>;

    fn init(&self) -> Data {
        Data { height: 0 }
    }

    fn mining<D: BlockDAG<Data>>(&self, d: D, ep: D::Block) -> (Self::BlockIter<D>, Data) {
        let parents = once(ep);
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
        d.parents(b).next()
    }

    fn progress<D: BlockDAG<Data>>(&self, d: D, b: D::Block) -> f32 {
        1.
    }

    type RewardAlloc<D: BlockDAG<Data>> = Once<(D::Miner, f32)>;

    fn reward<D: BlockDAG<Data>>(&self, d: D, b: D::Block) -> Self::RewardAlloc<D> {
        once((d.miner(b), 1.))
    }
}
