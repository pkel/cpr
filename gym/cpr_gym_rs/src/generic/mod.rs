// There are two parties to the game: the defenders and the attacker.

#[derive(Copy, Clone, Debug, PartialEq)]
enum Party {
    Attacker,
    Defender,
}

// Each block corresponds to one vertex in a directed acyclic graph (DAG).
//
// The defender has a restricted view on the DAG. Blocks are known or unknown. If a block is known
// all parents are known. Once blocks are known, they cannot become unknown. Exactly one known
// block is the defender's entry to the DAG.
//
// The attacker knows all blocks but may consider a only a subset. Once a block is considered it
// cannot become ignored. To consider a block, all parents have to be considered first. Exactly one
// considered block is the attacker's entry to the DAG.

#[derive(Copy, Clone, Debug, PartialEq)]
enum AView {
    Ignored,
    Considered,
    AEntry,
}
// attacker's view on a block

#[derive(Copy, Clone, Debug, PartialEq)]
enum DView {
    Unknown,
    Known,
    DEntry,
}
// defenders' view on a block

#[derive(Copy, Clone, Debug, PartialEq)]
enum NView {
    Honest,
    Withheld,
    Released,
}
// network's view on a block

use petgraph;

type BlockIx = petgraph::graph::DefaultIx;
type Block = petgraph::graph::NodeIndex<BlockIx>;

struct NodeWeight<P> {
    av: AView,
    dv: DView,
    nv: NView,
    pd: P, // protocol dependent data
}

type EdgeWeight = ();

type BlockDAG<ProtoData> = petgraph::graph::DiGraph<NodeWeight<ProtoData>, EdgeWeight>;

pub mod intf;

impl<'a, ProtoData> intf::BlockDAG<Block, Party, ProtoData> for &'a BlockDAG<ProtoData> {
    fn parents(&self, b: Block) -> Vec<Block> {
        self.neighbors_directed(b, petgraph::Direction::Outgoing)
            .collect()
    }

    fn children(&self, b: Block) -> Vec<Block> {
        self.neighbors_directed(b, petgraph::Direction::Incoming)
            .collect()
    }

    fn miner(&self, b: Block) -> Party {
        if self.node_weight(b).unwrap().nv == NView::Honest {
            Party::Defender
        } else {
            Party::Attacker
        }
    }

    fn data(&self, b: Block) -> &ProtoData {
        &self.node_weight(b).unwrap().pd
    }
}

// Both parties act honestly, that is according to the protocol specification, but on a subset of
// information. Why is this okay? I argue that any other misbehaviour leaves behind some evidence
// which enables non-interactive litigation.
//
// The following block filters define who sees what and what happens next.

// blocks visible to (honest, emulated) attacker node
fn bflt_a_sees<P>(nd: &NodeWeight<P>) -> bool {
    nd.av != AView::Ignored
}

// blocks visible to (honest, emulated) defender node
fn bflt_d_sees<P>(nd: &NodeWeight<P>) -> bool {
    nd.dv != DView::Unknown
}

// blocks about to be delivered to defender node
fn bflt_d_avail<P>(nd: &NodeWeight<P>) -> bool {
    nd.dv == DView::Unknown && nd.nv == NView::Released
}
// CAUTION: ensure that blocks are delivered in topographical order

// blocks available to the attacker for consideration
fn bflt_a_avail<P>(nd: &NodeWeight<P>) -> bool {
    nd.av == AView::Ignored
}
// CAUTION: ensure that blocks are considered in topographical order

// The BlockDAG is subject to some invariants, which I check below.

fn dag_check<ProtoData>(dag: BlockDAG<ProtoData>) {
    assert!(dag.node_count() > 0, "dag is empty");
    assert!(
        petgraph::algo::connected_components(&dag) == 1,
        "dag not connected"
    );
    assert!(!petgraph::algo::is_cyclic_directed(&dag), "not a dag");

    let mut aentry = 0;
    let mut dentry = 0;

    for n in dag.node_indices() {
        let nd = dag.node_weight(n).unwrap();

        // count entry points, there should be one each
        if nd.av == AView::AEntry {
            aentry += 1
        }
        if nd.dv == DView::DEntry {
            dentry += 1
        }

        // check topological visibility
        if bflt_a_sees(nd) {
            let f = |p| -> _ { bflt_a_sees(dag.node_weight(p).unwrap()) };
            assert!(dag.neighbors(n).all(f), "topological visibility")
        }
        if bflt_d_sees(nd) {
            let f = |p| -> _ { bflt_d_sees(dag.node_weight(p).unwrap()) };
            assert!(dag.neighbors(n).all(f), "topological visibility")
        }
    }

    assert!(aentry == 1, "ill-defined attacker entry point");
    assert!(dentry == 1, "ill-defined defender entry point");
}

// Partial views on the DAG

struct AttackerView<'a, ProtoData> {
    dag: &'a BlockDAG<ProtoData>,
}

impl<'a, ProtoData> intf::BlockDAG<Block, Party, ProtoData> for &'a AttackerView<'a, ProtoData> {
    fn parents(&self, b: Block) -> Vec<Block> {
        // parents are always visible, so we do not filter
        self.dag.parents(b)
    }

    fn children(&self, b: Block) -> Vec<Block> {
        let f = |x: &Block| self.dag.node_weight(*x).unwrap().av != AView::Ignored;
        self.dag
            .neighbors_directed(b, petgraph::Direction::Incoming)
            .filter(f)
            .collect()
    }

    fn miner(&self, b: Block) -> Party {
        self.dag.miner(b)
    }

    fn data(&self, b: Block) -> &ProtoData {
        self.dag.data(b)
    }
}

struct DefenderView<'a, ProtoData> {
    dag: &'a BlockDAG<ProtoData>,
}

impl<'a, ProtoData> intf::BlockDAG<Block, Party, ProtoData> for &'a DefenderView<'a, ProtoData> {
    fn parents(&self, b: Block) -> Vec<Block> {
        // parents are always visible, so we do not filter
        self.dag.parents(b)
    }

    fn children(&self, b: Block) -> Vec<Block> {
        let f = |x: &Block| self.dag.node_weight(*x).unwrap().dv != DView::Unknown;
        self.dag
            .neighbors_directed(b, petgraph::Direction::Incoming)
            .filter(f)
            .collect()
    }

    fn miner(&self, b: Block) -> Party {
        self.dag.miner(b)
    }

    fn data(&self, b: Block) -> &ProtoData {
        self.dag.data(b)
    }
}
