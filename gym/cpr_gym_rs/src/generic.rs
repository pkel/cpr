// There are two parties to the game: the defenders and the attacker.
//
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
type NodeData = (AView, DView, NView); // TODO maybe compress into single u8
type EdgeData = ();
type BlockDAG = petgraph::graph::DiGraph<NodeData, EdgeData>;

// Both parties act honestly, that is according to the protocol specification, but on a subset of
// information. Why is this okay? I argue that any other misbehaviour leaves behind some evidence
// which enables non-interactive litigation.
//
// The following block filters define who sees what and what happens next.

// blocks visible to (honest, emulated) attacker node
fn bflt_a_sees((av, _dv, _nv): NodeData) -> bool {
    av != AView::Ignored
}

// blocks visible to (honest, emulated) defender node
fn bflt_d_sees((_av, dv, _nv): NodeData) -> bool {
    dv != DView::Unknown
}

// blocks about to be delivered to defender node
fn bflt_d_avail((_av, dv, nv): NodeData) -> bool {
    dv == DView::Unknown && nv == NView::Released
}
// CAUTION: ensure that blocks are delivered in topographical order

// blocks available to the attacker for consideration
fn bflt_a_avail((av, _dv, _nv): NodeData) -> bool {
    av == AView::Ignored
}
// CAUTION: ensure that blocks are considered in topographical order

// The BlockDAG is subject to some invariants, which I check below.

fn dag_check(dag: BlockDAG) {
    assert!(dag.node_count() > 0, "dag is empty");
    assert!(
        petgraph::algo::connected_components(&dag) == 1,
        "dag not connected"
    );
    assert!(!petgraph::algo::is_cyclic_directed(&dag), "not a dag");

    let mut aentry = 0;
    let mut dentry = 0;

    for n in dag.node_indices() {
        let nd = *dag.node_weight(n).unwrap();
        let (av, dv, nv) = nd;

        // count entry points, there should be one each
        if av == AView::AEntry {
            aentry += 1
        }
        if dv == DView::DEntry {
            dentry += 1
        }

        // check topological visibility
        let a_closure = |p| -> _ { bflt_a_sees(*dag.node_weight(p).unwrap()) };
        let d_closure = |p| -> _ { bflt_d_sees(*dag.node_weight(p).unwrap()) };
        if bflt_a_sees(nd) {
            assert!(dag.neighbors(n).all(a_closure), "topological visibility")
        }
        if bflt_d_sees(nd) {
            assert!(dag.neighbors(n).all(d_closure), "topological visibility")
        }
    }

    assert!(aentry == 1, "ill-defined attacker entry point");
    assert!(dentry == 1, "ill-defined defender entry point");
}
