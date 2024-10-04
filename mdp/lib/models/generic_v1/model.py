from ...implicit_mdp import Model as ImplicitMDP
from ...implicit_mdp import Transition
from .protocols.interface import Protocol
from dataclasses import dataclass
from typing import Optional, NewType
import copy
import pynauty
import random

# ## BLOCK DAG


class DAG:
    def __init__(self) -> int:
        # blocks are numbers 0, 1, ...
        self._genesis = 0

        # we store the parent relationship as adjacency list ...
        self._parents = [[]]

        # ... and also maintain the inverse relation
        self._children = [set()]

        # ... and the height to have a topological ordering ready
        self._height = [0]

        # each block has a miner (except the genesis block)
        self._miner = [None]

        # the object can be frozen
        self._frozen = False

    def copy(self):
        new = self.__class__.__new__(self.__class__)
        new._genesis = self._genesis  # int
        new._parents = copy.deepcopy(self._parents)  # list[list[int]]
        new._children = copy.deepcopy(self._children)  # list[set[int]]
        new._height = self._height.copy()  # list[int]
        new._miner = self._miner.copy()  # list[int]
        new._frozen = False
        return new

    def freeze(self):
        if self._frozen:
            raise AttributeError("object already frozen")

        self._frozen = True

    @property
    def genesis(self) -> int:
        return self._genesis

    def size(self) -> int:
        return len(self._parents)

    def all_blocks(self) -> set[int]:
        return set(range(len(self._parents)))

    def blocks_of(self, miner) -> set[int]:
        return {b for b, m in enumerate(self._miner) if m == miner}

    def append(self, parents: list[int], miner: int) -> int:
        if self._frozen:
            raise AttributeError("cannot modify frozen object")

        b = len(self._parents)  # new block

        self._parents.append(parents)
        self._children.append(set())
        self._miner.append(miner)

        # track children relationship and height
        self._height.append(0)
        for p in parents:
            self._children[p].add(b)
            self._height[b] = max(self._height[b], self._height[p] + 1)

        return b

    def parents(self, block: int) -> list[int]:
        # the model guarantees that parents are always visible
        return self._parents[block]

    def children(self, block: int, subgraph: Optional[list[int]] = None) -> set[int]:
        if subgraph is None:
            return self._children[block]
        else:
            return self._children[block] & subgraph

    def miner_of(self, block: int) -> int:
        return self._miner[block]

    def height(self, block: int) -> int:
        return self._height[block]

    def topological_order(self, blocks: set[int]):
        return sorted(list(blocks))

    def _past_or_future(self, relation, block):
        acc = set()
        stack = set(relation(block))
        while len(stack) > 0:
            b = stack.pop()
            if b not in acc:
                acc.add(b)
                for p in set(relation(b)):
                    stack.add(p)
        return acc

    def past(self, block):
        return self._past_or_future(self.parents, block)

    def future(self, block):
        return self.past_or_future(self.children, block)


# ## MINERS


class DynObj:
    def __init__(self):
        self._frozen = False
        self._attributes = dict()

    def copy(self):
        new = self.__class__.__new__(self.__class__)
        new._attributes = copy.deepcopy(self._attributes)  # dict[string, Any]
        new._frozen = False
        return new

    def freeze(self):
        if self._frozen:
            raise AttributeError("object already frozen")

        self._frozen = True

    def __getattr__(self, name):
        if name.startswith("_"):
            super().__getattr__(name)
        elif name in self._attributes:
            return self._attributes[name]
        else:
            raise AttributeError(f"'DynObj' has not attribute '{name}'")

    def __setattr__(self, name, value):
        if name.startswith("_"):
            super().__setattr__(name, value)
        elif self._frozen:
            raise AttributeError("cannot modify frozen object")
        else:
            self._attributes[name] = value


# DynObj() objects allow to create attributes on first assign. We use this for
# the miners' state to give the protocol spec full authority over the attribute
# names.


class Miner:
    def __init__(self, dag: DAG, protocol: type[Protocol], *args, **kwargs):
        self._dag = dag
        self._protocol_fn = lambda: protocol(*args, **kwargs)

        self._visible = {dag.genesis}
        self._load_protocol_spec()

        # create and init miner's state as defined in protocol spec
        self._protocol.state = DynObj()
        self._protocol.init()

        self._frozen = False

    def copy_onto(self, dag):
        new = self.__class__.__new__(self.__class__)
        new._dag = dag
        new._protocol_fn = self._protocol_fn

        new._visible = self._visible.copy()
        new._load_protocol_spec()

        new._protocol.state = self._protocol.state.copy()

        new._frozen = False

        return new

    def _load_protocol_spec(self):
        # load protocol spec (sorry for the mess)
        self._protocol = self._protocol_fn()
        self._protocol.genesis = self._dag.genesis
        self._protocol.children = self.children
        self._protocol.parents = self.parents
        self._protocol.G = self._visible
        self._protocol.topological_order = self._dag.topological_order
        self._protocol.miner_of = self._dag.miner_of

    @property
    def visible(self):
        return self._visible.copy()

    @property
    def state(self):
        return self._protocol.state.copy()

    def freeze(self):
        if self._frozen:
            raise AttributeError("object already frozen")

        if not self._dag._frozen:
            raise AttributeError("cannot freeze miner on non-frozen DAG")

        self._protocol.state.freeze()
        self._frozen = True

    def parents(self, block: int):
        return self._dag.parents(block)

    def children(self, block: int):
        return self._dag.children(block, self._visible)

    def deliver(self, block: int) -> None:
        if self._frozen:
            raise AttributeError("cannot modify frozen object")

        # spec assumes each block is delivered once
        assert block not in self._visible, "deliver once"

        # spec assumes topologically ordered delivery
        assert all([p in self._visible for p in self._dag.parents(block)])

        # do the actual delivery
        self._visible.add(block)
        self._protocol.update(block)

    def mining(self) -> list[int]:
        set_or_list = self._protocol.mining()
        return list(set_or_list)

    def history(self) -> list[int]:
        return self._protocol.history()

    def coinbase(self, block):
        return self._protocol.coinbase(block)

    def progress(self, block):
        return self._protocol.progress(block)

    def relabel_state(self, new_ids):
        if self._frozen:
            raise AttributeError("cannot modify frozen object")

        self._protocol.relabel_state(new_ids)


# ## Attack Model
#   modeled after Sapirshtein et al. and later Bar-Zur et al.


@dataclass(frozen=True)
class Action:
    pass


@dataclass(frozen=True)
class Release(Action):
    block: int


@dataclass(frozen=True)
class Consider(Action):
    block: int


@dataclass(frozen=True)
class Continue(Action):
    pass


# imperative logic, actions modify state
class SingleAgentImp:
    def __init__(
        self, protocol: type[Protocol], *args, force_consider_own=False, **kwargs
    ):
        self.miner_fn = lambda dag: Miner(dag, protocol, *args, **kwargs)
        self.force_consider_own = force_consider_own

        self.dag = DAG()
        self.ignored = set()
        self.withheld = set()
        self.attacker = self.miner_fn(self.dag)
        self.defender = self.miner_fn(self.dag)

        # the object can be frozen
        self._frozen = False

    def freeze(self):
        if self._frozen:
            raise AttributeError("object already frozen")

        self.dag.freeze()
        self.attacker.freeze()
        self.defender.freeze()
        self._frozen = True

    def _collision_resistant_hash(self):
        raise NotImplementedError("TODO")

    @property
    def fingerprint(self):
        if not self._frozen:
            raise AttributeError("mutable agent; use .freeze() first")

        if not hasattr(self, "_fingerprint"):
            self._fingerprint = self._collision_resistant_hash()

        return self._fingerprint

    def __hash__(self):
        return hash(self.fingerprint)

    def __eq__(self, other):
        return self.fingerprint() == other.fingerprint()

    def to_release(self):
        # withheld blocks where no parent is withheld
        return {
            b
            for b in self.withheld
            if not any(p in self.withheld for p in self.dag.parents(b))
        }

    def do_release(self, b):
        if self._frozen:
            raise AttributeError("cannot modify frozen object")

        self.withheld.remove(b)

    def to_consider(self):
        # ignored blocks where no parent is ignored
        return {
            b
            for b in self.ignored
            if not any(p in self.ignored for p in self.dag.parents(b))
        }

    def do_consider(self, b):
        if self._frozen:
            raise AttributeError("cannot modify frozen object")

        self.ignored.remove(b)
        self.attacker.deliver(b)

    def just_released(self):
        # released attacker block not yet known to the defender
        return self.dag.blocks_of(0) - self.withheld - self.defender.visible

    def just_mined_by_defender(self):
        # defender blocks not yet known to the defender
        return self.dag.blocks_of(1) - self.defender.visible

    def do_communication(self, attacker_communicates_fast: bool):
        if self._frozen:
            raise AttributeError("cannot modify frozen object")

        just_released = sorted(self.just_released())
        just_mined_by_defender = sorted(self.just_mined_by_defender())
        if attacker_communicates_fast:
            to_deliver = just_released + just_mined_by_defender
        else:
            to_deliver = just_mined_by_defender + just_released

        # sort ensures delivery in topological order

        for b in to_deliver:
            self.defender.deliver(b)

    def do_mining(self, attacker_mines_next_block: bool):
        if self._frozen:
            raise AttributeError("cannot modify frozen object")

        if attacker_mines_next_block:
            parents = self.attacker.mining()
            b = self.dag.append(parents, 0)
            self.ignored.add(b)
            self.withheld.add(b)
        else:
            parents = self.defender.mining()
            b = self.dag.append(parents, 1)
            self.ignored.add(b)

    def apply(self, a: Action, *, gamma: float, alpha: float):
        if isinstance(a, Release):
            self.do_release(a.block)
        elif isinstance(a, Consider):
            self.do_consider(a.block)
        elif isinstance(a, Continue):
            self.do_communication(random.random() < gamma)
            self.do_mining(random.random() < alpha)
        else:
            assert isinstance(a, Action)
            assert False, "unknown action"

    def actions(self) -> set[Action]:
        acc = {Continue()}

        for b in sorted(self.to_consider()):
            # We simplify the model by forcing the attacker to consider its own
            # blocks.
            if self.force_consider_own and self.dag.miner_of(b) == 0:
                return {Consider(block=b)}

            acc.add(Consider(block=b))

        for b in self.to_release():
            acc.add(Release(block=b))

        return acc

    def _honest(self) -> Action:
        to_consider = sorted(self.to_consider())

        # We craft honest policy to overlap with possible actions.
        if self.force_consider_own:
            for b in to_consider:
                if self.dag.miner_of(b) == 0:
                    return Consider(block=b)

        to_release = sorted(self.to_release())
        if len(to_release) > 0:
            return Release(block=to_release[0])

        if len(to_consider) > 0:
            return Consider(block=to_consider[0])

        return Continue()

    def honest(self) -> Action:
        a = self._honest()
        assert a in self.actions()
        return a

    def do_shutdown(self, attacker_communicates_fast: bool):
        if self._frozen:
            raise AttributeError("cannot modify frozen object")

        self.withheld = set()
        self.do_communication(attacker_communicates_fast)

    def copy(self):
        new = self.__class__.__new__(self.__class__)
        new.miner_fn = self.miner_fn
        new.force_consider_own = self.force_consider_own
        new.dag = self.dag.copy()
        new.ignored = self.ignored.copy()
        new.withheld = self.withheld.copy()

        new.attacker = self.attacker.copy_onto(new.dag)
        new.defender = self.defender.copy_onto(new.dag)

        new._frozen = False

        return new

    def copy_and_relabel(self, prio: list[int]):
        # returns a copy with the blocks relabelled from 0 to self.size() - 1
        # following the priorities (high comes first) given.

        if len(prio) != self.dag.size():
            raise ValueError("size mismatch for list of priorities")

        # this class assumes that block ids are topologically ordered
        # we modify the priorities accordingly:

        prio = [(self.dag.height(b), -prio[b], b) for b in self.dag.all_blocks()]
        old_blocks_in_new_order = [b for _, _, b in sorted(prio)]
        new_ids = {b: i for i, b in enumerate(old_blocks_in_new_order)}

        # self.copy() + block renaming

        new = self.__class__.__new__(self.__class__)
        new.miner_fn = self.miner_fn
        new.force_consider_own = self.force_consider_own

        new.dag = DAG()
        for b in old_blocks_in_new_order:
            new_parents = [new_ids[p] for p in self.dag.parents(b)]
            miner = self.dag.miner_of(b)
            new.dag.append(new_parents, miner)

        new.ignored = {new_ids[b] for b in self.ignored}
        new.withheld = {new_ids[b] for b in self.withheld}

        new.attacker = self.attacker.copy_onto(new.dag)
        self.attacker.relabel_state(new_ids)

        new.defender = self.defender.copy_onto(new.dag)
        self.defender.relabel_state(new_ids)

        new._frozen = False

        return new

    def canonical_order(self):
        # see models/generic_v0/model.py:canocically_ordered for explanations

        g = pynauty.Graph(
            self.dag.size(),
            directed=True,
            adjacency_dict={b: self.dag.parents(b) for b in self.dag.all_blocks()},
            # vertex_coloring=vc, # TODO
        )

        # find canonical labels and return
        canon = pynauty.canon_label(g)

        return canon

    def copy_and_normalize(self):
        prio = self.canonical_order()
        return self.copy_and_relabel(prio)


State = NewType("State", SingleAgentImp)  # Py 3.12: type State = SingleAgentImp


class SingleAgent(ImplicitMDP):
    def __init__(self, protocol: type[Protocol], *args, alpha, gamma, **kwargs):
        assert 0 <= alpha <= 1
        assert 0 <= gamma <= 1
        self.alpha = alpha
        self.gamma = gamma

        self.start_attacker = SingleAgentImp(protocol, *args, **kwargs)
        self.start_attacker.do_mining(True)
        self.start_attacker.freeze()

        self.start_defender = SingleAgentImp(protocol, *args, **kwargs)
        self.start_defender.do_mining(False)
        self.start_defender.freeze()

    def start(self) -> list[tuple[State, float]]:
        """
        Define start states and initial probabilities.
        """
        return [
            (self.start_attacker, self.alpha),
            (self.start_defender, 1 - self.alpha),
        ]

    def actions(self, s: State) -> set[Action]:
        """
        Define valid actions.
        """
        return s.actions()

    def honest(self, s: State) -> Action:
        """
        What would an honest participant do?
        """
        return s.honest()

    def apply(self, a: Action, s: State) -> list[Transition]:
        """
        Define state transitions. Action a is applied to state s.
        """
        if isinstance(a, Release):
            cases = [(1.0, lambda s: s.do_release(a.block))]
            return self.finalize_transitions(s, cases)
        elif isinstance(a, Consider):
            cases = [(1.0, lambda s: s.do_consider(a.block))]
            return self.finalize_transitions(s, cases)
        elif isinstance(a, Continue):
            cases = [
                (
                    self.alpha * self.gamma,
                    lambda s: [s.do_communication(True), s.do_mining(True)],
                ),
                (
                    self.alpha * (1 - self.gamma),
                    lambda s: [s.do_communication(False), s.do_mining(True)],
                ),
                (
                    (1 - self.alpha) * self.gamma,
                    lambda s: [s.do_communication(True), s.do_mining(False)],
                ),
                (
                    (1 - self.alpha) * (1 - self.gamma),
                    lambda s: [s.do_communication(False), s.do_mining(False)],
                ),
            ]
            return self.finalize_transitions(s, cases)
        else:
            assert isinstance(a, Action)
            raise ValueError("unknown action")

    def shutdown(self, s: State) -> list[Transition]:
        """
        Define a fair shutdown mechanism. We call this at the end of each
        episode.

        Example. For selfish mining we force the agent to release all blocks,
        then do one last round of communication, before calculating the final
        rewards. This encourages risk-taking in the light of probabilistic
        termination.

        This does not correspond directly to terminal states in the associated
        MDP. This aborts any ongoing attack. It's okay to continue running the
        model after shutdown.
        """
        cases = [
            (
                self.gamma,
                lambda s: s.do_shutdown(True),
            ),
            (
                1 - self.gamma,
                lambda s: s.do_shutdown(False),
            ),
        ]
        return self.finalize_transitions(s, cases)

    def finalize_transitions(self, old, cases):
        # We measure the attacker's reward on the defender's chain
        # and calculate the delta between new and old state.

        def measure(hist, judge):
            rew = 0.0  # attacker's reward
            prg = 0.0  # progress
            for b in hist:
                prg += judge.progress(b)
                for miner, amount in judge.coinbase(b):
                    assert miner in [0, 1]
                    if miner == 0:
                        rew += amount

            return (rew, prg)

        old_hist = old.defender.history()
        assert old_hist[0] == 0  # genesis_check

        old_rew, old_prg = measure(old_hist[1:], old.defender)  # skip genesis

        transitions = []
        for prb, fn in cases:
            new = old.copy()
            fn(new)
            new.freeze()

            new_hist = new.defender.history()
            assert new_hist[0] == 0  # genesis check

            new_rew, new_prg = measure(new_hist[1:], new.defender)  # no genesis

            transitions.append(
                Transition(
                    probability=prb,
                    state=self.close_honest_loop(new, new_hist),
                    reward=new_rew - old_rew,
                    progress=new_prg - old_prg,
                    effect=None,
                )
            )

        return transitions

    def close_honest_loop(self, new, new_hist):
        # Our analysis relies on the honest policy looping on a closed set of states.
        # We apply a heuristic: if state looks honest, transition back to start.
        dag_size = new.dag.size()
        last_block = dag_size - 1

        def common(loop_state):
            # communication is complete
            assert len(new.attacker.visible) == dag_size - 1
            if len(new.defender.visible) != dag_size - 1:
                return new

            # history must be the same
            atk_hist = new.attacker.history()
            if len(atk_hist) != len(new_hist) or atk_hist != new_hist:
                return new

            # All blocks in the history must be confirmed by the last block of the history.
            # This might be relevant for GhostDAG.
            if set(new_hist[:-1]) != new.dag.past(new_hist[-1]):
                return new

            return loop_state

        # Case 1: attacker has mined the last block
        if (
            new.dag.miner_of(last_block) == 0
            and new.withheld == {last_block}
            and new.ignored == {last_block}
        ):
            return common(self.start_attacker)

        # Case 2: defender has mined the last block
        if (
            new.dag.miner_of(last_block) == 1
            and new.withheld == set()
            and new.ignored == {last_block}
        ):
            return common(self.start_defender)

        return new
