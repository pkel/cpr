from ...implicit_mdp import Model as ImplicitMDP
from ...implicit_mdp import Transition
from .protocols.interface import Protocol
from dataclasses import dataclass
from typing import Optional, NewType
import copy
import pynauty
import random
import subprocess
import xxhash

# ## BLOCK DAG


class DAG:
    def __init__(self) -> int:
        # we store the parent relationship as adjacency list ...
        self._parents = [set()]

        # ... and also maintain the inverse relation
        self._children = [set()]

        # ... and the height to have a topological ordering ready
        #     (height = distance to genesis)
        self._height = [0]

        # each block has a miner (except the genesis block)
        self._miner = [None]

        # the object can be frozen
        self._frozen = False

    def copy(self):
        new = self.__class__.__new__(self.__class__)
        new._parents = copy.deepcopy(self._parents)  # list[set[int]]
        new._children = copy.deepcopy(self._children)  # list[set[int]]
        new._height = self._height.copy()  # list[int]
        new._miner = self._miner.copy()  # list[int]
        new._frozen = False
        return new

    def freeze(self):
        if self._frozen:
            raise AttributeError("object already frozen")

        self._frozen = True

    def fingerprint(self):
        if not self._frozen:
            raise AttributeError("mutable DAG; use .freeze() first")

        assert self._parents[0] == set()
        assert self._miner[0] is None

        x = xxhash.xxh128()
        for i in range(1, len(self._parents)):  # skip genesis
            x.update(f";{i},{self._miner[i]}")
            for p in sorted(self._parents[i]):
                x.update(f",{p}")

        return x.digest()

    @property
    def genesis(self) -> int:
        return 0

    def size(self) -> int:
        return len(self._parents)

    def all_blocks(self) -> set[int]:
        return set(range(len(self._parents)))

    def blocks_of(self, miner) -> set[int]:
        return {b for b, m in enumerate(self._miner) if m == miner}

    def append(self, parents: set[int], miner: int) -> int:
        if self._frozen:
            raise AttributeError("cannot modify frozen object")

        assert isinstance(parents, set), "parents are not ordered"

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

    def parents(self, block: int) -> set[int]:
        # the model guarantees that parents are always visible
        return self._parents[block].copy()

    def children(self, block: int, subgraph: Optional[list[int]] = None) -> set[int]:
        if subgraph is None:
            return self._children[block].copy()
        else:
            return self._children[block] & subgraph

    def miner_of(self, block: int) -> int:
        assert block != self.genesis, "unsafe usage of miner_of"
        return self._miner[block]

    def height(self, block: int) -> int:
        return self._height[block]

    def topological_order(self, blocks: set[int]):
        with_height = [(self._height[b], b) for b in blocks]
        return list(map(lambda x: x[1], sorted(with_height)))

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
        return self._past_or_future(self.children, block)


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

    def fingerprint(self):
        if not self._frozen:
            raise AttributeError("mutable object; use .freeze() first")

        x = xxhash.xxh128()
        for i, (k, v) in enumerate(sorted(self._attributes.items())):
            x.update(f";{i},{k},{v}")

        return x.digest()

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

    def __repr__(self):
        return super().__repr__() + ": " + str(self._attributes)


# DynObj() objects allow to create attributes on first assign. We use this for
# the miners' state to give the protocol spec full authority over the attribute
# names.


class Miner:
    def __init__(self, dag: DAG, protocol: type[Protocol], *args, id, **kwargs):
        self._dag = dag
        self._protocol_fn = lambda: protocol(*args, **kwargs)
        self._id = id

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
        new._id = self._id

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
        self._protocol.height = self._dag.height
        self._protocol.miner_of = self._dag.miner_of
        self._protocol.me = self._id

    def fingerprint(self):
        if not self._frozen:
            raise AttributeError("mutable miner; use .freeze() first")

        x = xxhash.xxh128()
        for i, b in enumerate(sorted(self._visible)):
            x.update(f";{i},{b}")
        x.update(self._protocol.state.fingerprint())

        return x.digest()

    @property
    def visible(self):
        return frozenset(self._visible)

    @property
    def state(self):
        return self._protocol.state

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

    def mining(self) -> set[int]:
        return self._protocol.mining()

    def history(self) -> list[int]:
        return self._protocol.history()

    def coinbase(self, block):
        return self._protocol.coinbase(block)

    def progress(self, block):
        return self._protocol.progress(block)

    def color_block(self, block):
        return self._protocol.color_block(block)

    def relabel_state(self, new_ids):
        if self._frozen:
            raise AttributeError("cannot modify frozen object")

        # relabel elements of _visible w/o creating a new set as the set is
        # linked to self._protocol
        tmp = self._visible.copy()
        self._visible.clear()
        for b in tmp:
            if b in new_ids:
                self._visible.add(new_ids[b])

        self._protocol.relabel_state(new_ids)

    def collect_garbage(self):
        return self._protocol.collect_garbage()


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
        self._force_consider_own = force_consider_own

        self._dag = DAG()
        self._ignored = set()
        self._withheld = set()
        self._attacker = Miner(self._dag, protocol, *args, **kwargs, id=0)
        self._defender = Miner(self._dag, protocol, *args, **kwargs, id=1)

        # the object can be frozen
        self._frozen = False

    def freeze(self):
        if self._frozen:
            raise AttributeError("object already frozen")

        self._dag.freeze()
        self._attacker.freeze()
        self._defender.freeze()
        self._frozen = True

    @property
    def dag(self):
        return self._dag

    @property
    def attacker(self):
        return self._attacker

    @property
    def defender(self):
        return self._defender

    @property
    def withheld(self):
        return self._withheld.copy()

    @property
    def ignored(self):
        return self._ignored.copy()

    def _fingerprint(self):
        x = xxhash.xxh128()

        x.update(self._dag.fingerprint())
        x.update(self._attacker.fingerprint())
        x.update(self._defender.fingerprint())
        for b in sorted(self._withheld):
            x.update(f",{b}")
        x.update(";")
        for b in sorted(self._ignored):
            x.update(f",{b}")

        return x.digest()

    @property
    def fingerprint(self):
        if not self._frozen:
            raise AttributeError("mutable agent; use .freeze() first")

        assert self._dag._frozen
        assert self._attacker._frozen
        assert self._defender._frozen

        if not hasattr(self, "_cached_fingerprint"):
            self._cached_fingerprint = self._fingerprint()

        return self._cached_fingerprint

    def __hash__(self):
        return hash(self.fingerprint)

    def __eq__(self, other):
        return self.fingerprint == other.fingerprint

    def to_release(self):
        # withheld blocks where no parent is withheld
        return {
            b
            for b in self._withheld
            if not any(p in self._withheld for p in self._dag.parents(b))
        }

    def do_release(self, b):
        if self._frozen:
            raise AttributeError("cannot modify frozen object")

        self._withheld.remove(b)

    def to_consider(self):
        # ignored blocks where no parent is ignored
        return {
            b
            for b in self._ignored
            if not any(p in self._ignored for p in self._dag.parents(b))
        }

    def do_consider(self, b):
        if self._frozen:
            raise AttributeError("cannot modify frozen object")

        self._ignored.remove(b)
        self._attacker.deliver(b)

    def just_released(self):
        # released attacker block not yet known to the defender
        return self._dag.blocks_of(0) - self._withheld - self._defender.visible

    def just_mined_by_defender(self):
        # defender blocks not yet known to the defender
        return self._dag.blocks_of(1) - self._defender.visible

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
            self._defender.deliver(b)

    def do_mining(self, attacker_mines_next_block: bool):
        if self._frozen:
            raise AttributeError("cannot modify frozen object")

        if attacker_mines_next_block:
            parents = self._attacker.mining()
            b = self._dag.append(parents, 0)
            self._ignored.add(b)
            self._withheld.add(b)
            if self._force_consider_own:
                self.do_consider(b)
        else:
            parents = self._defender.mining()
            b = self._dag.append(parents, 1)
            self._ignored.add(b)

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

        for b in self.to_consider():
            acc.add(Consider(block=b))

        for b in self.to_release():
            acc.add(Release(block=b))

        return acc

    def _honest(self) -> Action:
        to_consider = self._dag.topological_order(self.to_consider())
        if len(to_consider) > 0:
            return Consider(block=to_consider[0])

        to_release = self._dag.topological_order(self.to_release())
        if len(to_release) > 0:
            return Release(block=to_release[0])

        return Continue()

    def honest(self) -> Action:
        a = self._honest()
        assert a in self.actions()
        return a

    def do_shutdown(self, attacker_communicates_fast: bool):
        if self._frozen:
            raise AttributeError("cannot modify frozen object")

        self._withheld = set()
        self.do_communication(attacker_communicates_fast)

    def copy(self):
        new = self.__class__.__new__(self.__class__)
        new._force_consider_own = self._force_consider_own
        new._dag = self._dag.copy()
        new._ignored = self._ignored.copy()
        new._withheld = self._withheld.copy()

        new._attacker = self._attacker.copy_onto(new._dag)
        new._defender = self._defender.copy_onto(new._dag)

        new._frozen = False

        return new

    def copy_and_relabel(self, order: list[int], *, strict=True):
        # Returns a copy with the blocks relabelled according to the order
        # given.

        # This may be used to remove blocks from the DAG, e.g. when truncating
        # common history.

        if strict and len(order) != self._dag.size():
            raise ValueError("size mismatch for ordering")

        if strict and set(order) != self._dag.all_blocks():
            raise ValueError("order does not cover all block ids")

        new_heights = [self._dag.height(b) for b in order]
        if sorted(new_heights) != new_heights:
            raise ValueError("order is not topological")

        # ---

        new_ids = {b: i for i, b in enumerate(order)}

        new = self.__class__.__new__(self.__class__)
        new._force_consider_own = self._force_consider_own

        new._dag = DAG()
        assert not strict or order[0] == self._dag.genesis

        for b in order[1:]:
            new_parents = {new_ids[p] for p in self._dag.parents(b)}
            miner = self._dag.miner_of(b)
            new._dag.append(new_parents, miner)

        new._ignored = {new_ids[b] for b in self._ignored if b in new_ids}
        new._withheld = {new_ids[b] for b in self._withheld if b in new_ids}

        new._attacker = self._attacker.copy_onto(new._dag)
        new._attacker.relabel_state(new_ids)

        new._defender = self._defender.copy_onto(new._dag)
        new._defender.relabel_state(new_ids)

        new._frozen = False

        return new

    def canonical_order(self, *, colors=None):
        # see models/generic_v0/model.py:canonically_ordered for explanations

        if colors is None:
            coloring = dict()
        else:
            assert len(colors) == self._dag.size()

            color_sets = dict()
            for b, c in enumerate(colors):
                if c not in color_sets:
                    color_sets[c] = set()
                color_sets[c].add(b)

            vc = [color_sets[c] for c in sorted(color_sets.keys())]

            coloring = dict(vertex_coloring=vc)

        g = pynauty.Graph(
            self._dag.size(),
            directed=True,
            adjacency_dict={
                b: sorted(self._dag.parents(b)) for b in self._dag.all_blocks()
            },
            **coloring,
        )

        canon_label = pynauty.canon_label(g)

        assert isinstance(canon_label, list)
        assert isinstance(canon_label[0], int)
        assert len(canon_label) == self._dag.size()

        old_blocks_in_canonical_order = canon_label

        # In principle we have a canonical ordering now. It does not respect an
        # important invariant of the DAG class though: block ids are
        # topologically ordered. We fix this here. A deterministic permutation
        # of a canonical ordering is still canonical!

        new_positions_of_old_blocks = {
            old: new for new, old in enumerate(old_blocks_in_canonical_order)
        }

        prioritized_blocks = [
            (
                self._dag.height(b),
                new_positions_of_old_blocks[b],
                self._dag.miner_of(b) if b != self._dag.genesis else -1,
                b,
            )
            for b in self._dag.all_blocks()
        ]

        return [b for _, _, _, b in sorted(prioritized_blocks)]

    def copy_and_normalize(self):
        colors = [0]  # genesis
        colors += [self._dag.miner_of(b) for b in range(1, self._dag.size())]
        bits = 1

        for b in self._defender.visible:
            colors[b] |= 1 << bits
        bits += 1

        for b in self._attacker.visible:
            colors[b] |= 1 << bits
        bits += 1

        for b in self._withheld:
            colors[b] |= 1 << bits
        bits += 1

        for b in self._ignored:
            colors[b] |= 1 << bits
        bits += 1

        for b in self._defender.visible:
            dcol = self._defender.color_block(b)
            assert 0 <= dcol <= 1
            colors[b] |= dcol << bits
        bits += 1

        for b in self._attacker.visible:
            dcol = self._attacker.color_block(b)
            assert 0 <= dcol <= 1
            colors[b] |= dcol << bits
        bits += 1

        order = self.canonical_order(colors=colors)
        return self.copy_and_relabel(order)

    def graph_easy(self, info=dict()):
        lbls = []
        lns = []
        for b in self._dag.all_blocks():
            if b == self._dag.genesis:
                lbl = f"{b}: genesis"
            elif self._dag.miner_of(b) == 0:
                lbl = f"{b}: atk"
            else:
                lbl = f"{b}: def"

            if b in self._ignored:
                lbl += ", ign"

            if b in self._withheld:
                lbl += ", whd"

            lbls.append(lbl)
            lns.append(f"[{lbls[-1]}]")

        for b in self._dag.all_blocks():
            for p in self._dag.parents(b):
                lns.append(f"[{lbls[b]}] --> [{lbls[p]}]")
        return "\n".join(lns)

    def asciify(self, info=dict()):
        rendered = subprocess.run(
            ["graph-easy"], input=self.graph_easy(info), text=True, capture_output=True
        )
        rendered.check_returncode()

        ret = rendered.stdout
        ret += "attacker: " + str(self._attacker.state) + "\n"
        ret += "defender: " + str(self._defender.state)
        return ret

    def debug_print(self, info=dict()):
        print(self.asciify(info))

    def __repr__(self):
        return super().__repr__() + ":\n" + self.asciify()


State = NewType("State", SingleAgentImp)  # Py 3.12: type State = SingleAgentImp


class SingleAgent(ImplicitMDP):
    def __init__(
        self,
        protocol: type[Protocol],
        *args,
        alpha,
        gamma,
        collect_garbage=False,  # "judge", "simple", None, True (=simple), False (=None)
        dag_size_cutoff=None,  # int; force abort attack at dag size
        loop_honest=False,
        merge_isomorphic=False,
        reward_common_chain=False,  # instead of defender chain
        traditional_height_cutoff=None,  # int; force abort attack at height
        truncate_common_chain=False,
        **kwargs,
    ):
        assert 0 <= alpha <= 1
        assert 0 <= gamma <= 1
        self.alpha = alpha
        self.gamma = gamma
        self.dag_size_cutoff = dag_size_cutoff
        self.loop_honest = loop_honest
        self.merge_isomorphic = merge_isomorphic
        self.reward_common_chain = reward_common_chain
        self.traditional_height_cutoff = traditional_height_cutoff
        self.truncate_common_chain = truncate_common_chain

        if isinstance(collect_garbage, bool):
            if collect_garbage:
                self.collect_garbage = "simple"
            else:
                self.collect_garbage = None
        else:
            self.collect_garbage = collect_garbage

        if truncate_common_chain and loop_honest:
            raise ValueError("choose either truncate_common_chain or loop_honest")

        if reward_common_chain and not truncate_common_chain:
            raise ValueError("reward_common_chain requires truncate_common_chain")

        if self.loop_honest:
            # prepare two honest states to loop back to
            self.reset_attacker = SingleAgentImp(protocol, *args, **kwargs)
            self.reset_attacker.do_mining(True)
            self.reset_defender = SingleAgentImp(protocol, *args, **kwargs)
            self.reset_defender.do_mining(False)

            if merge_isomorphic:
                self.reset_attacker = self.reset_attacker.copy_and_normalize()
                self.reset_defender = self.reset_defender.copy_and_normalize()

            self.reset_attacker.freeze()
            self.reset_defender.freeze()
        else:
            self.start_state = SingleAgentImp(protocol, *args, **kwargs)
            if merge_isomorphic:
                self.start_state = self.start_state.copy_and_normalize()
            self.start_state.freeze()

    def start(self) -> list[tuple[State, float]]:
        """
        Define start states and initial probabilities.
        """
        if self.loop_honest:
            return [
                (self.reset_attacker, self.alpha),
                (self.reset_defender, 1 - self.alpha),
            ]
        else:
            return [(self.start_state, 1.0)]

    def actions(self, s: State) -> set[Action]:
        """
        Define valid actions.
        """

        # Force honest behavior beyond a certain DAG height / size.
        # Note: The traditional models (fc16/aft20) force aborting the attack
        # by forbidding the Wait action. We could do the same here, by
        # forbidding the Continue action. But this does not work here, because
        # the Release action is executed only with the next Continue step.
        if self.traditional_height_cutoff is not None:
            max_height = max(s.dag.height(b) for b in s.dag.all_blocks())
            if max_height >= self.traditional_height_cutoff:
                return {self.honest(s)}
        if self.dag_size_cutoff is not None:
            if s.dag.size() >= self.dag_size_cutoff:
                return {self.honest(s)}

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

        if not self.reward_common_chain:  # reward defender chain
            # We measure the attacker's reward on the defender's chain
            # and calculate the delta between new and old state.

            old_hist = old.defender.history()
            assert old_hist[0] == 0  # genesis_check

            # skip genesis; it has been considered before
            old_rew, old_prg = measure(old_hist[1:], old.defender)

        transitions = []
        for prb, fn in cases:
            new = old.copy()
            fn(new)
            new.freeze()

            if not self.reward_common_chain:  # reward defender chain
                new_hist = new.defender.history()
                assert new_hist[0] == new.dag.genesis

                new_rew, new_prg = measure(new_hist[1:], new.defender)  # no genesis
                rew = new_rew - old_rew
                prg = new_prg - old_prg

            if self.collect_garbage:
                # TODO avoid redundant copy
                new = self.copy_and_collect_garbage(new)

            assert not (self.loop_honest and self.truncate_common_chain)

            if self.loop_honest:
                new = self.loop_honest_to_start(new)

            if self.truncate_common_chain:
                # TODO avoid redundant copy
                pre = new
                post, truncated_upto = self.loop_truncate_common_chain(pre)

                if self.reward_common_chain:
                    # calculate reward/progress on truncated chain
                    if truncated_upto == pre.dag.genesis:
                        # no truncation happened
                        rew, prg = 0.0, 0.0
                    else:
                        hist = []
                        assert truncated_upto in pre.defender.history()[1:]
                        for b in pre.defender.history()[1:]:  # skip old genesis
                            hist.append(b)
                            if b == truncated_upto:
                                break  # include new genesis
                        rew, prg = measure(hist, pre.defender)

                new = post

            if self.merge_isomorphic:
                # TODO avoid redundant copy
                new = new.copy_and_normalize()
                new.freeze()

            transitions.append(
                Transition(
                    probability=prb,
                    state=new,
                    reward=rew,
                    progress=prg,
                    effect=None,
                )
            )

        return transitions

    def collect_garbage_simple(self, state):
        # we keep blocks
        # - which are not visible to either party
        # - which are marked relevant by either party
        # - the closure w.r.t the parents relationship

        keep = set()

        all_blocks = state.dag.all_blocks()
        keep |= all_blocks - state.defender.visible
        keep |= all_blocks - state.attacker.visible
        keep |= state.attacker.collect_garbage()
        keep |= state.defender.collect_garbage()

        for b in keep.copy():
            keep |= state.dag.past(b)

        return keep

    def collect_garbage_judge(self, state):
        # we keep blocks
        # - which are marked relevant by either party
        # - that the defender would keep after learning about all blocks
        # - the closure w.r.t the parents relationship

        judge = state.defender.copy_onto(state.dag)
        missing_blocks = state.dag.all_blocks() - judge.visible
        for b in state.dag.topological_order(missing_blocks):
            judge.deliver(b)

        keep = judge.collect_garbage()
        keep |= state.attacker.collect_garbage()
        keep |= state.defender.collect_garbage()

        for b in keep.copy():
            keep |= state.dag.past(b)

        return keep

    def copy_and_collect_garbage(self, state):
        if self.collect_garbage == "simple":
            keep = self.collect_garbage_simple(state)
        elif self.collect_garbage == "judge":
            keep = self.collect_garbage_judge(state)
        elif self.collect_garbage is None:
            raise Exception("collecting garbage although self.collect_garbage is None")
        else:
            raise Exception(
                "invalid value for self.collect_garbage: " + repr(self.collect_garbage)
            )

        ordered_keep = state.dag.topological_order(keep)

        cleaned = state.copy_and_relabel(ordered_keep, strict=False)
        cleaned.freeze()
        return cleaned

    def loop_honest_to_start(self, new):
        # Our analysis relies on the honest policy looping on a closed set of states.
        # We apply a heuristic: if state looks honest, transition back to start.
        # TODO this heuristic does not work for the parallel protocol!
        dag_size = new._dag.size()
        last_block = dag_size - 1

        def_hist = new.defender.history()

        def common(loop_state):
            # communication is complete
            assert len(new.attacker.visible) == dag_size - 1
            if len(new.defender.visible) != dag_size - 1:
                return new

            # history must be the same
            atk_hist = new.attacker.history()
            if len(atk_hist) != len(def_hist) or atk_hist != def_hist:
                return new

            # All blocks in the history must be confirmed by the last block of the history.
            # This might be relevant for GhostDAG.
            if set(def_hist[:-1]) != new.dag.past(def_hist[-1]):
                return new

            return loop_state

        # Case 1: attacker has mined the last block
        if (
            new.dag.miner_of(last_block) == 0
            and new.withheld == {last_block}
            and new.ignored == {last_block}
        ):
            return common(self.reset_attacker)

        # Case 2: defender has mined the last block
        if (
            new.dag.miner_of(last_block) == 1
            and new.withheld == set()
            and new.ignored == {last_block}
        ):
            return common(self.reset_defender)

        return new

    def loop_truncate_common_chain(self, state):
        atk_hist = state.attacker.history()
        def_hist = state.defender.history()

        assert atk_hist[0] == state.dag.genesis == def_hist[0]

        # Find latest block on common history that can serve as the new
        # genesis block. We will remove its past. This should not change the
        # semantics of the DAG and it should leave behind a single root block.
        # git blame this for more information.

        next_genesis = state.dag.genesis

        for i in range(1, min(len(atk_hist), len(def_hist))):
            b = atk_hist[i]
            if b != def_hist[i]:
                # histories have diverged; no further truncation possible
                break

            past = state.dag.past(b)
            past_and_b = {b} | past

            b_is_viable = True
            for pb in past:
                if not all(c in past_and_b for c in state.dag.children(pb)):
                    b_is_viable = False
                    break
            if b_is_viable:
                next_genesis = b

        # ---
        # truncate past of next_genesis

        if next_genesis == state.dag.genesis:
            # no truncation happens
            return (state, state.dag.genesis)

        subset = {next_genesis} | state.dag.future(next_genesis)

        assert subset == state.dag.all_blocks() - state.dag.past(next_genesis)

        ordered_subset = state.dag.topological_order(subset)
        assert ordered_subset[0] == next_genesis
        truncated = state.copy_and_relabel(ordered_subset, strict=False)
        truncated.freeze()
        return (truncated, next_genesis)
