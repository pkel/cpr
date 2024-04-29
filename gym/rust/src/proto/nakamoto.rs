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

pub struct BaseObserver {}

// This reproduces the state/observation of the Nakamoto model in fc16.rs.
// It has three fields:
// - a: the length of the attacker's chain, relative to the common ancestor
// - d: the length of the defender's chain, relative to the common ancestor
// - fork: whether a match might be successful or not, i.e. whether the attacker has available a
//   withheld block that matches the height of a defender block just mined. If so, this block can
//   be released, and with probability gamma overwrites the defenders' new block.
impl<Block, Miner> intf::FeatureExtractor<Block, Miner, Data> for BaseObserver
where
    Block: Copy,
    Miner: Copy,
{
    fn observe<DAG: BlockDAG<Block, Miner, Data>>(
        &self,
        d: &DAG,
        atk: Block,
        def: Block,
        ca: Block,
        known_to_all_defenders: &dyn Fn(Block) -> bool,
        mined_by_defender: &dyn Fn(Block) -> bool,
    ) -> Vec<f32> {
        let ch = d.data(ca).height; // height common ancestor
        let ah = d.data(atk).height; // height attacker preferred block
        let mut dh = d.data(def).height; // height defender preferred block
        let mut mf = false; // match feasible?

        // a freshly mined defender block is not yet the preferred block, in other words, there
        // might be a child to def.
        // TODO FeatureExtractor API leaks peculiarities of the Env.
        let children: Vec<Block> = d
            .children(def)
            .into_iter()
            .filter(|&b| mined_by_defender(b))
            .collect();
        if children.len() > 0 {
            assert!(children.len() == 1, "bug in protocol spec or env?");
            let def = children[0];
            assert!(d.children(def).len() == 0, "bug in proto spec or env?");
            assert!(!known_to_all_defenders(def), "bug in proto spec or env?");
            assert!(d.data(def).height == dh + 1, "bug in proto spec or env?");
            dh += 1;
            mf = true;
        }

        vec![(ah - ch) as f32, (dh - ch) as f32, mf as u8 as f32]
    }

    fn low(&self) -> Vec<f32> {
        vec![0., 0., 0.]
    }

    fn high(&self) -> Vec<f32> {
        vec![<f32>::MAX, <f32>::MAX, 1.]
    }
}
