from ...implicit_mdp import Model as ImplicitMDP
from ...implicit_mdp import Transition
from .protocols.interface import Protocol
from dataclasses import dataclass
from typing import Optional, NewType
import copy
import random

# ## BLOCK DAG


class DAG:
    def __init__(self) -> int:
        # blocks are numbers 0, 1, ...
        self.genesis = 0

        # we store the parent relationship as adjacency list ...
        self._parents = [[]]

        # ... and also maintain the inverse relation
        self._children = [set()]

        # each block has a miner (except the genesis block)
        self._miner = [None]

    def size(self) -> int:
        return len(self._parents)

    def all_blocks(self) -> set[int]:
        return set(range(len(self._parents)))

    def blocks_of(self, miner) -> set[int]:
        return {b for b, m in enumerate(self._miner) if m == miner}

    def append(self, parents: list[int], miner: int) -> int:
        new_block = len(self._parents)

        self._parents.append(parents)
        self._children.append(set())
        self._miner.append(miner)

        for p in parents:
            self._children[p].add(new_block)

        return new_block

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
    def __setattr__(self, name, value):
        self.__dict__[name] = value


# DynObj() objects allow to create attributes on first assign. We use this for
# the miners' state to give the protocol spec full authority over the attribute
# names.


class Miner:
    def __init__(self, dag: DAG, protocol: type[Protocol], *args, **kwargs):
        self.dag = dag

        # initialize local visibility
        self.visible = {self.dag.genesis}

        # load protocol spec
        self.protocol = protocol(*args, **kwargs)
        self.protocol.genesis = self.dag.genesis
        self.protocol.children = self.children
        self.protocol.parents = self.dag.parents
        self.protocol.G = self.visible
        self.protocol.topological_order = self.dag.topological_order
        self.protocol.miner_of = self.dag.miner_of

        # create and init miner's state as defined in protocol spec
        self.protocol.state = DynObj()
        self.protocol.init()

    def children(self, block: int):
        return self.dag.children(block, self.visible)

    def deliver(self, block: int) -> None:
        # spec assumes each block is delivered once
        assert block not in self.visible, "deliver once"

        # spec assumes topologically ordered delivery
        assert all([p in self.visible for p in self.dag.parents(block)])

        # do the actual delivery
        self.visible.add(block)
        self.protocol.update(block)

    def mining(self) -> list[int]:
        set_or_list = self.protocol.mining()
        return list(set_or_list)

    def history(self) -> list[int]:
        return self.protocol.history()

    def coinbase(self, block):
        return self.protocol.coinbase(block)

    def progress(self, block):
        return self.protocol.progress(block)


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
    def __init__(self, protocol: type[Protocol], *args, **kwargs):
        self.miner_fn = lambda dag: Miner(dag, protocol, *args, **kwargs)

        self.dag = DAG()
        self.ignored = set()
        self.withheld = set()
        self.attacker = self.miner_fn(self.dag)
        self.defender = self.miner_fn(self.dag)

    def to_release(self):
        # withheld blocks where no parent is withheld
        return {
            b
            for b in self.withheld
            if not any(p in self.withheld for p in self.dag.parents(b))
        }

    def do_release(self, b):
        self.withheld.remove(b)

    def to_consider(self):
        # ignored blocks where no parent is ignored
        return {
            b
            for b in self.ignored
            if not any(p in self.ignored for p in self.dag.parents(b))
        }

    def do_consider(self, b):
        self.ignored.remove(b)
        self.attacker.deliver(b)

    def just_released(self):
        # released attacker block not yet known to the defender
        return self.dag.blocks_of(0) - self.withheld - self.defender.visible

    def just_mined_by_defender(self):
        # defender blocks not yet known to the defender
        return self.dag.blocks_of(1) - self.defender.visible

    def do_communication(self, attacker_communicates_fast: bool):
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

        for b in self.to_release():
            acc.add(Release(block=b))

        for b in self.to_consider():
            acc.add(Consider(block=b))

        return acc

    def honest(self) -> Action:
        to_release = self.to_release()
        if len(to_release) > 0:
            return Release(block=to_release.pop())

        to_consider = self.to_consider()
        if len(to_consider) > 0:
            return Consider(block=to_consider.pop())

        return Continue()

    def do_shutdown(self, attacker_communicates_fast: bool):
        self.withheld = set()
        self.do_communication(attacker_communicates_fast)

    def copy(self):
        new = self.__class__.__new__(self.__class__)
        new.miner_fn = self.miner_fn
        new.dag = copy.deepcopy(self.dag)
        new.ignored = self.ignored.copy()
        new.withheld = self.withheld.copy()

        new.attacker = self.miner_fn(new.dag)
        new.attacker.protocol.state = copy.deepcopy(self.attacker.protocol.state)
        for b in self.attacker.visible:
            new.attacker.visible.add(b)

        new.defender = self.miner_fn(new.dag)
        new.defender.protocol.state = copy.deepcopy(self.defender.protocol.state)
        for b in self.defender.visible:
            new.defender.visible.add(b)

        return new


State = NewType("State", SingleAgentImp)  # Py 3.12: type State = SingleAgentImp


class SingleAgent(ImplicitMDP):
    def __init__(self, protocol: type[Protocol], *args, alpha, gamma, **kwargs):
        assert 0 <= alpha <= 1
        assert 0 <= gamma <= 1
        self.alpha = alpha
        self.gamma = gamma

        self.start_attacker = SingleAgentImp(protocol, *args, **kwargs)
        self.start_attacker.do_mining(True)

        self.start_defender = SingleAgentImp(protocol, *args, **kwargs)
        self.start_defender.do_mining(False)

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
