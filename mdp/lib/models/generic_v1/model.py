from typing import Optional

### BLOCK DAG


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


### MINERS


class DynObj:
    def __setattr__(self, name, value):
        self.__dict__[name] = value


# DynObj() objects allow to create attributes on first assign. We use this for
# the miners' state to give the protocol spec full authority over the attribute
# names.

from .protocols.interface import Protocol


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


### Attack Model
#   modeled after Sapirshtein et al. and later Bar-Zur et al.

from dataclasses import dataclass
import random


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
        self.dag = DAG()
        self.ignored = set()
        self.withheld = set()
        self.attacker = Miner(self.dag, protocol, *args, **kwargs)
        self.defender = Miner(self.dag, protocol, *args, **kwargs)

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

    def actions(self) -> list[Action]:
        acc = [Continue()]

        for b in self.to_release():
            acc.append(Release(block=b))

        for b in self.to_consider():
            acc.append(Consider(block=b))

        return acc

    def honest(self) -> Action:
        to_release = self.to_release()
        if len(to_release) > 0:
            return Release(block=to_release.pop())

        to_consider = self.to_consider()
        if len(to_consider) > 0:
            return Consider(block=to_consider.pop())

        return Continue()


from ...implicit_mdp import Model as ImplicitMDP
from ...implicit_mdp import Transition, Effect


@dataclass(frozen=True)
class State:
    dag: DAG
    ignored: set[int]
    withheld: set[int]


# TODO implement class SingleAgent(ImplicitMDP); it's only a template so far
# functional logic, actions return updated state
class SingleAgent(ImplicitMDP):
    def __init__(self, protocol: type[Protocol], *args, **kwargs):
        self.protocol_fn = protocol
        self.protocol_args = args
        self.protocol_kwargs = kwargs
        raise NotImplementedError

    def start(self) -> list[tuple[State, float]]:
        """
        Define start states and initial probabilities.
        """
        raise NotImplementedError

    def actions(self, s: State) -> set[Action]:
        """
        Define valid actions.
        """
        raise NotImplementedError

    def apply(self, a: Action, s: State) -> list[Transition]:
        """
        Define state transitions. Action a is applied to state s.
        """
        raise NotImplementedError

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
        raise NotImplementedError

    def acc_effect(self, a: Effect, b: Effect) -> Effect:
        """
        When merging two steps, what's the accumulated effect?
        """
        if a is None and b is None:
            return None
        else:
            raise NotImplementedError

    def honest(self, s: State) -> Action:
        """
        What would an honest participant do?
        """
        raise NotImplementedError
