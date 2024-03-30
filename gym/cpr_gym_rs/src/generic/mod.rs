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

type NodeIx = petgraph::graph::DefaultIx;
type Node = petgraph::graph::NodeIndex<NodeIx>;
type Block = Node;

struct NodeWeight<P> {
    av: AView,
    dv: DView,
    nv: NView,
    pd: P, // protocol dependent data
}

type EdgeWeight = ();

type Graph<ProtoData> = petgraph::graph::DiGraph<NodeWeight<ProtoData>, EdgeWeight>;

pub mod intf;
use intf::{BlockDAG, Protocol};

impl<ProtoData> BlockDAG<Block, Party, ProtoData> for Graph<ProtoData> {
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

fn dag_check<ProtoData>(g: Graph<ProtoData>) {
    assert!(g.node_count() > 0, "dag is empty");
    assert!(
        petgraph::algo::connected_components(&g) == 1,
        "dag not connected"
    );
    assert!(!petgraph::algo::is_cyclic_directed(&g), "not a dag");

    let mut aentry = 0;
    let mut dentry = 0;

    for n in g.node_indices() {
        let nd = g.node_weight(n).unwrap();

        // count entry points, there should be one each
        if nd.av == AView::AEntry {
            aentry += 1
        }
        if nd.dv == DView::DEntry {
            dentry += 1
        }

        // check topological visibility
        if bflt_a_sees(nd) {
            let f = |p| -> _ { bflt_a_sees(g.node_weight(p).unwrap()) };
            assert!(g.neighbors(n).all(f), "topological visibility")
        }
        if bflt_d_sees(nd) {
            let f = |p| -> _ { bflt_d_sees(g.node_weight(p).unwrap()) };
            assert!(g.neighbors(n).all(f), "topological visibility")
        }
    }

    assert!(aentry == 1, "ill-defined attacker entry point");
    assert!(dentry == 1, "ill-defined defender entry point");
}

// Partial views on the DAG

struct AttackerView<ProtoData> {
    g: Graph<ProtoData>,
}

impl<ProtoData> BlockDAG<Block, Party, ProtoData> for AttackerView<ProtoData> {
    fn parents(&self, b: Block) -> Vec<Block> {
        // parents are always visible, so we do not filter
        self.g.parents(b)
    }

    fn children(&self, b: Block) -> Vec<Block> {
        let f = |x: &Block| self.g.node_weight(*x).unwrap().av != AView::Ignored;
        self.g
            .neighbors_directed(b, petgraph::Direction::Incoming)
            .filter(f)
            .collect()
    }

    fn miner(&self, b: Block) -> Party {
        self.g.miner(b)
    }

    fn data(&self, b: Block) -> &ProtoData {
        self.g.data(b)
    }
}

struct DefenderView<ProtoData> {
    g: Graph<ProtoData>,
}

impl<ProtoData> BlockDAG<Block, Party, ProtoData> for DefenderView<ProtoData> {
    fn parents(&self, b: Block) -> Vec<Block> {
        // parents are always visible, so we do not filter
        self.g.parents(b)
    }

    fn children(&self, b: Block) -> Vec<Block> {
        let f = |x: &Block| self.g.node_weight(*x).unwrap().dv != DView::Unknown;
        self.g
            .neighbors_directed(b, petgraph::Direction::Incoming)
            .filter(f)
            .collect()
    }

    fn miner(&self, b: Block) -> Party {
        self.g.miner(b)
    }

    fn data(&self, b: Block) -> &ProtoData {
        self.g.data(b)
    }
}

// Action Space
//
// The attacker may
//  - withhold own blocks, i.e. not release them to the defender, and
//  - ignore any block, i.e. not consider them when extending the chain.
//
// We impose some topological restrictions. These follow from the topological restrictions in the
// DAG views. In practice, it follows from the natural restriction that verifying hash references
// requires availability of all parents blocks. Blocks must be
//   - released after their parents, and
//   - considered after their parents.
//
// When looking at a regular protocol execution, most blocks will be released and considered
// already. Even under attack, only a few blocks are 'actionable'. E.g., in Bitcoin, when looking
// at a period where the attacker maintains a long private fork, at most three blocks are
// actionable (the first ignored/ignored/withheld defender/attacker/attacker block can be
// considered/considered/released).
//
// We thus expose action of the form
//  - `Release i`. Among all blocks that can be released, release the i-th option.
//  - `Consider i`. Among all blocks that can be considered, consider the i-th option.
//
// The release action merely flips `AView` enum of the block.
// A third action `Continue` triggers the next simulation step, which
//  - (a) concludes the previous communication between the defending nodes
//  - (b) sends the to be released blocks to the defending nodes
//  - (c) advances in time until the next block is mined.
// Steps (a) and (b) might happen in reverse order depending on the network assumptions.
//
// We encode the actions as signed integers:
//
// - negative integers: -i means `Release  i`
// - positive integers:  i means `Consider i`
// - zero:               0 means `Continue`

struct AvailableActions {
    release: Vec<Block>,
    consider: Vec<Block>,
}

fn available_actions<P>(g: Graph<P>) -> AvailableActions {
    let mut release = vec![];
    let mut consider = vec![];

    // iterate all blocks
    for b in g.node_indices() {
        let nw = g.node_weight(b).unwrap();

        // check release possible
        if nw.nv == NView::Withheld
            && g.parents(b)
                .into_iter()
                .all(|p| g.node_weight(p).unwrap().nv != NView::Withheld)
        {
            release.push(b);
        }

        // check consider possible
        if nw.av == AView::Ignored
            && g.parents(b)
                .into_iter()
                .all(|p| g.node_weight(p).unwrap().av != AView::Ignored)
        {
            consider.push(b);
        }
    }

    AvailableActions { release, consider }
}

// Initial DAG

fn init<P, D>(p: P) -> Graph<D>
where
    P: Protocol<Graph<D>, Block, Party, D>,
{
    let mut g = Graph::new();
    let genesis = NodeWeight {
        av: AView::AEntry,
        dv: DView::DEntry,
        nv: NView::Honest,
        pd: p.init(),
    };
    g.add_node(genesis);
    g
}
