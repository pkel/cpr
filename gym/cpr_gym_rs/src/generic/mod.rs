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

struct PartialView<'a, ProtoData> {
    g: &'a Graph<ProtoData>,                    // unfiltered graph
    p: fn(&'a Graph<ProtoData>, Block) -> bool, // filter property
}

impl<'a, ProtoData> BlockDAG<Block, Party, ProtoData> for PartialView<'a, ProtoData> {
    fn parents(&self, b: Block) -> Vec<Block> {
        // parents are always visible, so we do not filter
        self.g.parents(b)
    }

    fn children(&self, b: Block) -> Vec<Block> {
        let f = |x: &Block| self.g.node_weight(*x).unwrap().dv != DView::Unknown;
        self.g
            .neighbors_directed(b, petgraph::Direction::Incoming)
            .filter(|&x| (self.p)(self.g, x))
            .collect()
    }

    fn miner(&self, b: Block) -> Party {
        self.g.miner(b)
    }

    fn data(&self, b: Block) -> &ProtoData {
        self.g.data(b)
    }
}

fn attacker_view<'a, P>(g: &'a Graph<P>) -> PartialView<'a, P> {
    let p = |g: &Graph<P>, x: Block| g.node_weight(x).unwrap().av != AView::Ignored;
    PartialView { g, p }
}

fn defender_view<'a, P>(g: &'a Graph<P>) -> PartialView<'a, P> {
    let p = |g: &Graph<P>, x: Block| g.node_weight(x).unwrap().dv != DView::Unknown;
    PartialView { g, p }
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

fn available_actions<P>(g: &Graph<P>) -> AvailableActions {
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

// randomness

use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

// environment logic

struct Env<P, D>
where
    P: Protocol<Block, Party, D>,
{
    g: Graph<D>,
    p: P,
    a: AvailableActions,
    alpha: f64,
    gamma: f64,
    rng: StdRng,
}

fn new_graph<P, D>(p: &P) -> Graph<D>
where
    P: Protocol<Block, Party, D>,
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

impl<P, D> BlockDAG<Block, Party, D> for Env<P, D>
where
    P: Protocol<Block, Party, D>,
{
    fn parents(&self, b: Block) -> Vec<Block> {
        self.g.parents(b)
    }
    fn children(&self, b: Block) -> Vec<Block> {
        self.g.children(b)
    }
    fn data(&self, b: Block) -> &D {
        self.g.data(b)
    }
    fn miner(&self, b: Block) -> Party {
        self.g.miner(b)
    }
}

impl<P, D> Env<P, D>
where
    P: Protocol<Block, Party, D>,
{
    fn init_graph(&mut self) {
        let genesis = NodeWeight {
            av: AView::AEntry,
            dv: DView::DEntry,
            nv: NView::Honest,
            pd: self.p.init(),
        };
        self.g.add_node(genesis);
    }

    fn reset(&mut self) {
        self.g.clear();
        self.init_graph();
        self.a = available_actions(&self.g);
    }

    fn new(p: P, alpha: f64, gamma: f64) -> Self {
        let g = new_graph(&p);
        let a = available_actions(&g);
        Env {
            g,
            p,
            a,
            alpha,
            gamma,
            rng: StdRng::from_entropy(),
        }
        // TODO consider seeding
    }

    fn weight(&self, b: Block) -> &NodeWeight<D> {
        self.g.node_weight(b).unwrap()
    }

    fn mut_weight(&mut self, b: Block) -> &mut NodeWeight<D> {
        self.g.node_weight_mut(b).unwrap()
    }

    fn entrypoint(&self, m: Party) -> Block {
        if m == Party::Defender {
            self.g
                .node_indices()
                .find(|&p| self.weight(p).dv == DView::DEntry)
                .unwrap()
        } else {
            self.g
                .node_indices()
                .find(|&p| self.weight(p).av == AView::AEntry)
                .unwrap()
        }
    }

    fn deliver(&mut self, b: Block) {
        assert!(self.weight(b).dv == DView::Unknown, "repeated delivery");
        assert!(
            self.parents(b)
                .into_iter()
                .all(|p| { self.weight(p).dv != DView::Unknown }),
            "missing dependencies"
        );
        // turn visible
        self.mut_weight(b).dv = DView::Known;
        // apply update
        let ep = self.entrypoint(Party::Defender);
        self.mut_weight(ep).dv = DView::Known; // downgrade from DEntry
        let view = defender_view(&self.g);
        let ep = self.p.update(&view, ep, b);
        self.mut_weight(ep).dv = DView::DEntry; // upgrade from Known
    }

    fn consider(&mut self, b: Block) {
        assert!(self.weight(b).av == AView::Ignored, "repeated consider");
        assert!(
            self.parents(b)
                .into_iter()
                .all(|p| { self.weight(p).av != AView::Ignored }),
            "missing dependencies"
        );
        // turn visible
        self.mut_weight(b).av = AView::Considered;
        // apply update
        let ep = self.entrypoint(Party::Attacker);
        self.mut_weight(ep).av = AView::Considered; // downgrade from DEntry
        let view = defender_view(&self.g);
        let ep = self.p.update(&view, ep, b);
        self.mut_weight(ep).av = AView::AEntry; // upgrade from Known
    }

    fn release(&mut self, b: Block) {
        assert!(self.weight(b).nv == NView::Withheld, "unsuitable block");
        assert!(
            self.parents(b)
                .into_iter()
                .all(|p| { self.weight(p).nv != NView::Withheld }),
            "missing dependencies"
        );
        // mark released
        self.mut_weight(b).nv = NView::Released;
    }

    fn just_released(&self) -> Vec<Block> {
        self.g
            .node_indices()
            .filter(|&x| {
                let nd = self.weight(x);
                nd.dv == DView::Unknown && nd.nv == NView::Released
            })
            .collect()
    }

    fn just_mined_by_defender(&self) -> Vec<Block> {
        self.g
            .node_indices()
            .filter(|&x| {
                let nd = self.weight(x);
                nd.dv == DView::Unknown && nd.nv == NView::Honest
            })
            .collect()
    }

    fn continue_(&mut self) {
        self.communicate();
        // mining depends on network assumptions
        if self.attacker_mines_next_block() {
            self.mine_attacker()
        } else {
            self.mine_defender()
        }
    }

    fn attacker_communicates_fast(&mut self) -> bool {
        let x: f64 = self.rng.gen();
        x <= self.gamma
    }

    fn attacker_mines_next_block(&mut self) -> bool {
        let x: f64 = self.rng.gen();
        x <= self.alpha
    }

    fn communicate(&mut self) {
        // blocks just released by attacker
        let from_attacker = self.just_released();
        // blocks just mined by defender
        let from_defender = self.just_mined_by_defender();
        assert!(from_defender.len() <= 1, "abnormal honest mining");
        // communication depends on network assumption
        if self.attacker_communicates_fast() {
            for b in from_attacker {
                // TODO enforce topological order
                self.deliver(b)
            }
            for b in from_defender {
                self.deliver(b)
            }
        } else {
            for b in from_defender {
                // TODO enforce topological order
                self.deliver(b)
            }
            for b in from_attacker {
                self.deliver(b)
            }
        }
    }

    fn mine_defender(&mut self) {
        let view = defender_view(&self.g);
        let ep = self.entrypoint(Party::Defender);
        let (parents, pd) = self.p.mining(&view, ep);
        let nw = NodeWeight {
            av: AView::Ignored,
            dv: DView::Unknown,
            nv: NView::Honest,
            pd,
        };
        let b = self.g.add_node(nw);
        for p in parents {
            self.g.update_edge(b, p, ());
        }
    }

    fn mine_attacker(&mut self) {
        let view = attacker_view(&self.g);
        let ep = self.entrypoint(Party::Attacker);
        let (parents, pd) = self.p.mining(&view, ep);
        let nw = NodeWeight {
            av: AView::Ignored,
            dv: DView::Unknown,
            nv: NView::Withheld,
            pd,
        };
        let b = self.g.add_node(nw);
        for p in parents {
            self.g.update_edge(b, p, ());
        }
    }

    fn step(&mut self, a: i32) {
        // decode action
        if a < 0 {
            // lookup in available action
            let b = self.a.release[usize::try_from(-a).unwrap()];
            self.release(b)
        } else if a > 0 {
            // lookup in available action
            let b = self.a.release[usize::try_from(a - 1).unwrap()];
            self.consider(b)
        } else {
            // a == 0
            self.continue_()
        }
        self.a = available_actions(&self.g)
        // TODO implement termination
    }
}
