struct Nakamoto {}

use crate::generic::intf::{BlockDAG, Protocol};

use std::iter::{Once, once};

impl Protocol for Nakamoto {
    type BlockIter<D:BlockDAG> = Once<D::Block>;

    fn mining<D: BlockDAG>(&self, d: D, ep: D::Block) -> Self::BlockIter<D> {
        once(ep)
    }

    fn update<D: BlockDAG>(&self, d: D, ep: D::Block, b: D::Block) -> D::Block {
        if d.height(b) > d.height(ep) {
            b
        } else {
            ep
        }
    }

    fn tip<D: BlockDAG>(&self, d: D, ep: D::Block) -> D::Block {
        ep
    }

    fn pred<D: BlockDAG>(&self, d: D, b: D::Block) -> D::Block {
        d.parents(b).next().unwrap()
    }

    fn progress<D: BlockDAG>(&self, d: D, b: D::Block) -> f32 {
        1.
    }

    type RewardAlloc<D: BlockDAG> = Once<(D::Miner, f32)>;

    fn reward<D: BlockDAG>(&self, d: D, b: D::Block) -> Self::RewardAlloc<D> {
        once((d.miner(b), 1.))
    }
}
