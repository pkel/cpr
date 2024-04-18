use std::collections::VecDeque;

// There are two parties to the game: the defenders and the attacker.

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Party {
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
pub type Block = Node;

struct NodeWeight<P> {
    av: AView,
    dv: DView,
    nv: NView,
    pd: P, // protocol dependent data
}

type EdgeWeight = ();

type Graph<ProtoData> = petgraph::graph::DiGraph<NodeWeight<ProtoData>, EdgeWeight>;

pub mod intf;
use intf::{BlockDAG, FeatureExtractor, Protocol};

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

fn dag_check<ProtoData>(g: &Graph<ProtoData>) -> bool {
    assert!(g.node_count() > 0, "dag is empty");
    assert!(
        petgraph::algo::connected_components(g) == 1,
        "dag not connected"
    );
    assert!(!petgraph::algo::is_cyclic_directed(g), "not a dag");

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

    true
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
//
// We cap i <= 255, i.e. unsigned 8bit.
//
// When sampling from these random actions uniformly it will happen that the Continue is drowned in
// (mostly invalid) Release and Consider actions. We avoid this by mapping the integer actions to
// real numbers between -1 and 1, giving actions close to 0 more weight.
// The exposed action space is (-1, 1).

pub type Action = f32; // (-1., 1.)

#[derive(Clone, Copy, Debug)]
pub enum ActionHum {
    Release(u8),
    Consider(u8),
    Continue,
}

pub fn encode_action(a: ActionHum) -> Action {
    match a {
        ActionHum::Release(x) => {
            let x = -<f32>::from(x) - 1.;
            x / (1. + x.abs())
        }
        ActionHum::Consider(x) => {
            let x = <f32>::from(x) + 1.;
            x / (1. + x.abs())
        }
        ActionHum::Continue => 0.,
    }
}

pub fn decode_action(a: Action) -> ActionHum {
    assert!(a >= -1., "invalid action: {a} outside [-1, 1]");
    assert!(a <= 1., "invalid action: {a} outside [-1, 1]");

    if a == -1. {
        return ActionHum::Release(<u8>::MAX);
    }

    if a == 1. {
        return ActionHum::Consider(<u8>::MAX);
    }

    let x = if a >= 0. { -a / (a - 1.) } else { a / (a + 1.) };

    let x = x.round();

    if x < -256. {
        ActionHum::Release(<u8>::MAX)
    } else if x < 0. {
        // -256 <= x <= 1
        ActionHum::Release((-x - 1.) as u8)
    } else if x > 256. {
        ActionHum::Consider(<u8>::MAX)
    } else if x > 0. {
        // 1 <= x <= 256
        ActionHum::Consider((x - 1.) as u8)
    } else {
        ActionHum::Continue
    }
}

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

pub struct Env<P, D, O>
where
    P: Protocol<Block, Party, D>,
    O: FeatureExtractor<Block, Party, D>,
{
    g: Graph<D>,
    p: P,
    o: O,
    a: AvailableActions,
    alpha: f32,
    gamma: f32,
    horizon: f32,
    rng: StdRng,
    ca: Block,        // cached common ancestor
    hist: Vec<Block>, // linear history on defender's chain
}

fn init_graph<P, D>(g: &mut Graph<D>, p: &P) -> Block
where
    P: Protocol<Block, Party, D>,
{
    let genesis = NodeWeight {
        av: AView::AEntry,
        dv: DView::DEntry,
        nv: NView::Honest,
        pd: p.init(),
    };
    g.add_node(genesis)
}

impl<P, D, O> BlockDAG<Block, Party, D> for Env<P, D, O>
where
    P: Protocol<Block, Party, D>,
    O: FeatureExtractor<Block, Party, D>,
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

pub enum InfoV {
    F32(f32),
    USize(usize),
}

pub type Info = Vec<(&'static str, InfoV)>;

impl<P, D, O> Env<P, D, O>
where
    P: Protocol<Block, Party, D>,
    O: FeatureExtractor<Block, Party, D>,
{
    pub fn new(p: P, o: O, alpha: f32, gamma: f32, horizon: f32) -> Self {
        let mut g = Graph::new();
        let genesis = init_graph(&mut g, &p);
        let a = available_actions(&g);
        Env {
            g,
            p,
            o,
            a,
            alpha,
            gamma,
            horizon,
            rng: StdRng::from_entropy(),
            ca: genesis,
            hist: vec![genesis],
        }
    }

    pub fn describe(&self) -> String {
        format!(
            "Generic {{ alpha: {}, gamma: {}, horizon: {:?} }}",
            self.alpha, self.gamma, self.horizon,
        )
        // TODO: possible actions
    }

    fn reset(&mut self) {
        self.g.clear();
        let genesis = init_graph(&mut self.g, &self.p);
        self.a = available_actions(&self.g);
        self.ca = genesis;
        self.hist = vec![genesis];
    }

    fn guarded_action(&self, a: ActionHum) -> ActionHum {
        match a {
            ActionHum::Continue => ActionHum::Continue,
            ActionHum::Release(i) => {
                let l = self.a.release.len();
                if l < 1 {
                    ActionHum::Continue
                } else if usize::from(i) >= l {
                    let i: u8 = (l - 1).try_into().unwrap();
                    ActionHum::Release(i)
                } else {
                    a
                }
            }
            ActionHum::Consider(i) => {
                let l = self.a.consider.len();
                if l < 1 {
                    ActionHum::Continue
                } else if usize::from(i) >= l {
                    let i: u8 = (l - 1).try_into().unwrap();
                    ActionHum::Consider(i)
                } else {
                    a
                }
            }
        }
    }

    fn step(&mut self, a: Action) -> (f32, bool, bool, Info) {
        // check dag invariants
        assert!(dag_check(&self.g));

        // decode action & apply
        let mut time = 0;
        match self.guarded_action(decode_action(a)) {
            ActionHum::Release(i) => {
                let idx: usize = i.into();
                self.release(self.a.release[idx])
            }
            ActionHum::Consider(i) => {
                let idx: usize = i.into();
                self.consider(self.a.consider[idx])
            }
            ActionHum::Continue => {
                self.continue_();
                time = 1
            }
        }
        let time = time;

        // enumerate next set of actions
        self.a = available_actions(&self.g);

        // cache common ancestor; observation and next step rely on this
        let h = self.common_history();
        self.ca = *h.last().unwrap();
        // TODO: remove common chain stuff, as it is irrelevant now.
        // We might still need it though to expose fc16.rs-like observations.

        // probabilistic termination and rewards
        //
        // We follow closely fc16.rs and model long-term revenue.
        // Others metrics are possible. TODO
        // [ ] relative reward
        //     progress := attacker + defender reward
        //     reward := attacker reward
        // [ ] short term revenue
        //     progress := blocks mined
        //     reward := attacker reward
        // [x] long term revenue
        //     progress := protocol-defined progress # TODO rename to difficulty contribution
        //     reward := attacker reward
        // [ ] history rewriting
        //     progress := blocks mined or blocks
        //     reward := number of blocks rewritten

        let (mut rewrite, mut prg, mut attacker_rew, mut defender_rew) =
            self.track_defender_chain();

        // PTO assumption: non-negative progress
        // negative progress implies that defender accepted a lower progress chain.
        assert!(prg >= 0., "negative progress, invalid protocol?");
        let term = self.env_terminates(prg);

        // Force full information on termination
        //
        // Without this mechanism:
        //   - We track the defender chain not the common chain
        //   - We operate with probabilistic termination
        //   - Certain states, e.g. withholding a strictly longer chain, are risk-free in the real
        //     world but not in this model
        //   - The agent might be forced to play it safe and release block early in the model
        //   - This puts more emphasis on the honest strategy
        //   - Especially on short horizons
        //
        // With this mechanism the final episode rewards are computed on the complete BlockDAG. The
        // agent does not have to worry about withholding beneficial information beyond the end of
        // the episode.
        //
        // I observe that a hand-coded selfish mining policy against Nakamoto yields significantly
        // higher rewards after introducing this mechanism.
        if term {
            self.shutdown();

            // track changes on defender chain
            let (rewrite_t, prg_t, attacker_rew_t, defender_rew_t) = self.track_defender_chain();

            // accumulate reward metrics
            rewrite += rewrite_t;
            prg += prg_t;
            attacker_rew += attacker_rew_t;
            defender_rew += defender_rew_t

            // NOTE this creates yet another progress metric. `prg` includes `prg_t` but `prg_t` is
            // not counted towards probabilistic termination. Probabilistic termination is based on
            // `prg - prg_t`. `prg_t` is zero most of the times but not during the last step of the
            // episode.
            //
            // I think this is fine but cannot fully rule out unintended effects.
        }

        // we do not use truncation currently
        let trunc = false;

        let info = vec![
            ("progress", InfoV::F32(prg)),
            ("reward_attacker", InfoV::F32(attacker_rew)),
            ("reward_defender", InfoV::F32(defender_rew)),
            ("rewrite", InfoV::USize(rewrite)),
            ("time", InfoV::USize(time)),
        ];

        // scale reward
        let rew = attacker_rew / self.alpha / self.horizon;

        // return
        (<f32>::try_from(rew).unwrap(), term, trunc, info)
    }

    fn track_defender_chain(&mut self) -> (usize, f32, f32, f32) {
        // The PTO paper makes an assumption (A. 3 on p. 4) that any policy implies a strictly
        // positive expected difficulty contribution. This restricts how we can compute progress in
        // the simulator.
        //  - ok: one progress per block mined, represents time in the short term, i.e. before
        //    difficulty adjustment
        //  - ok: accumulated progress on defender chain, represents time in the long term, i.e.
        //    after difficulty adjustment.
        //  - not ok: accumulated progress on attacker or common chain as these depend on the
        //    policy. The "always continue" policy implies that attacker and common chain do not
        //    grow.
        // With that in mind I do here some tracking of the defender's linear history.
        // get defender's current and old history

        let cur = self.history(self.p.tip(&self.g, self.entrypoint(Party::Defender)));
        let old = &self.hist;

        // walk through histories: setup
        let cur_len = cur.len();
        let old_len = old.len();
        assert!(cur[0] == old[0], "genesis must not change");
        assert!(cur_len >= old_len, "shrinking history; protocol bug?",);

        // walk through histories: ignore common prefix
        let mut i = 0;
        while i < old_len && cur[i] == old[i] {
            i += 1;
        }
        let first = i;

        // walk through histories: track current progress and reward
        let mut prg = 0.;
        let mut rew_atk = 0.;
        let mut rew_def = 0.;
        let mut i = first;
        while i < cur_len {
            prg += self.p.progress(&self.g, cur[i]);
            for (m, x) in self.p.reward(&self.g, cur[i]) {
                match m {
                    Party::Attacker => rew_atk += x,
                    Party::Defender => rew_def += x,
                }
            }
            i += 1;
        }

        // walk through histories: track updated history entries and subtract old progress and
        // reward
        let mut upd = 0;
        let mut i = first;
        while i < old_len {
            upd += 1;
            prg -= self.p.progress(&self.g, old[i]);
            for (m, x) in self.p.reward(&self.g, old[i]) {
                match m {
                    Party::Attacker => rew_atk -= x,
                    Party::Defender => rew_def -= x,
                }
            }
            i += 1;
        }

        // store defender's new history for next iteration
        self.hist = cur;

        (upd, prg, rew_atk, rew_def)
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
            // we simplify the RL problem, forcing the attacker to consider own blocks
            let b = self.mine_attacker();
            self.consider(b)
        } else {
            self.mine_defender()
        }
    }

    // release all withheld blocks & communicate one last time
    fn shutdown(&mut self) {
        let mut from_attacker = vec![];
        let mut from_defender = vec![];

        for b in self.g.node_indices() {
            let nw = self.weight(b);
            let nv = nw.nv;
            let dv = nw.dv;

            if nv == NView::Withheld {
                self.release(b);
                from_attacker.push(b)
            }

            if nv == NView::Honest && dv == DView::Unknown {
                from_defender.push(b)
            }
        }

        // only one defender block should be on the wire at any time
        assert!(from_defender.len() <= 1, "env logic broken?");

        // order of delivery depends on network assumptions
        if self.attacker_communicates_fast() {
            from_attacker.into_iter().for_each(|b| self.deliver(b));
            from_defender.into_iter().for_each(|b| self.deliver(b));
        } else {
            from_defender.into_iter().for_each(|b| self.deliver(b));
            from_attacker.into_iter().for_each(|b| self.deliver(b));
        }
    }

    fn attacker_communicates_fast(&mut self) -> bool {
        let x: f32 = self.rng.gen();
        x <= self.gamma
    }

    fn attacker_mines_next_block(&mut self) -> bool {
        let x: f32 = self.rng.gen();
        x <= self.alpha
    }

    // Bar-Zur et al. write at AFT '20 on page 5:
    //
    // "In order to create a memoryless termination probability we utilize independent coin tosses.
    // For every one unit of accumulated [progress], the MDP tosses a coin: Terminate the process
    // with probability 1 - 1/H or continue with probability 1 - 1/H . Intuitively, since each unit
    // of accumulated [progress] causes termination with probability 1/H the accumulated [progress]
    // resembles a geometric distribution and therefore in expectation would be H once termination
    // occurs.
    //
    // "4.3.2 The Auxiliary MDP. For an ARR-MDP we denote its auxiliary MDP by PT-MDP . PT-MDP has
    // the same state space as ARR-MDP , with an additional terminal state with zero reward. PT-MDP
    // is parametrized by some chosen parameter H . At every time step t, the agent in PT-MDP has a
    // probability of 1 - (1 - 1/H)^[progress] , to transition to the terminal state. If
    // termination has not occurred, then the transition occurs as in ARR-MDP. Intuitively, the
    // process continues only if all independent coin tosses (there are [progress] of them)
    // indicate to continue.
    //
    // In their setting, progress is discrete. We have floats here, but I think this distinction is
    // not important.
    fn env_terminates(&mut self, progress: f32) -> bool {
        let x: f32 = self.rng.gen();
        let prob = 1. - (1. - 1. / self.horizon).powf(progress);
        x <= prob
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
                // topological order follows from increasing block indices
                self.deliver(b)
            }
            for b in from_defender {
                self.deliver(b)
            }
        } else {
            for b in from_defender {
                // topological order follows from increasing block indices
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

    fn mine_attacker(&mut self) -> Block {
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
        b
    }

    fn history(&self, b: Block) -> Vec<Block> {
        let mut v = VecDeque::new();
        let mut ptr = Some(b);
        while let Some(i) = ptr {
            v.push_front(i);
            ptr = self.p.pred(&self.g, i)
        }
        v.into()
    }

    fn common_history(&self) -> Vec<Block> {
        let a = self.history(self.p.tip(&self.g, self.entrypoint(Party::Attacker)));
        let d = self.history(self.p.tip(&self.g, self.entrypoint(Party::Defender)));
        assert!(a.len() > 0, "empty history");
        assert!(d.len() > 0, "empty history");
        assert!(a[0] == d[0]);

        let mut common = Vec::new();
        let max_len = std::cmp::min(a.len(), d.len());
        for i in 0..max_len {
            let x = a[i];
            if x == d[i] {
                common.push(x)
            } else {
                break;
            }
        }
        common
    }

    fn observe(&self) -> Vec<f32> {
        // apply protocol-dependent feature extractor
        let mut obs: Vec<f32> = {
            let atk = self.entrypoint(Party::Attacker);
            let def = self.entrypoint(Party::Defender);
            let ktad = |b: Block| -> bool { self.weight(b).dv != DView::Unknown };
            let mbd = |b: Block| -> bool { self.weight(b).nv == NView::Honest };
            self.o.observe(&self.g, atk, def, self.ca, &ktad, &mbd)
        };

        // add information about available actions
        // a) release
        let n = self.a.release.len();
        if n > 0 {
            obs.push(encode_action(ActionHum::Release((n - 1) as u8)))
        } else {
            obs.push(0.)
        }
        // b) consider
        let n = self.a.consider.len();
        if n > 0 {
            obs.push(encode_action(ActionHum::Consider((n - 1) as u8)))
        } else {
            obs.push(0.)
        }

        obs
    }

    fn low(&self) -> Vec<f32> {
        let mut obs = self.o.low();
        obs.push(-1.); // mininum release action
        obs.push(0.); // minimum consider action
        obs
    }

    fn high(&self) -> Vec<f32> {
        let mut obs = self.o.high();
        obs.push(0.); // maximum release action
        obs.push(1.); // maximum consider action
        obs
    }
}

// We need a slightly different interface for Python-interop

use numpy::IntoPyArray;
use pyo3::prelude::*;
use std::collections::HashMap;

impl IntoPy<PyObject> for InfoV {
    fn into_py(self, py: Python) -> PyObject {
        match self {
            InfoV::F32(x) => x.into_py(py),
            InfoV::USize(x) => x.into_py(py),
        }
    }
}

impl<P, D, O> Env<P, D, O>
where
    P: Protocol<Block, Party, D>,
    O: FeatureExtractor<Block, Party, D>,
{
    fn py_observe(&self, py: Python) -> PyObject {
        self.observe().into_pyarray(py).into()
    }

    fn py_info(&self, py: Python, i: Info) -> HashMap<String, PyObject> {
        let mut hm = HashMap::new();
        for (k, v) in i {
            hm.insert(k.into(), v.into_py(py));
        }
        hm
    }

    pub fn py_low(&self, py: Python) -> PyObject {
        self.low().into_pyarray(py).into()
    }

    pub fn py_high(&self, py: Python) -> PyObject {
        self.high().into_pyarray(py).into()
    }

    pub fn py_reset(&mut self, py: Python) -> (PyObject, HashMap<String, PyObject>) {
        self.reset();
        (self.py_observe(py), HashMap::new())
    }

    pub fn py_step(
        &mut self,
        py: Python,
        a: Action,
    ) -> (PyObject, f32, bool, bool, HashMap<String, PyObject>) {
        let (rew, term, trunc, info) = self.step(a);
        (
            self.py_observe(py),
            rew,
            term,
            trunc,
            self.py_info(py, info),
        )
    }
}
