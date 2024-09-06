from typing import Callable, Optional

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
        return {b for b, _ in enumerate(self._parents)}

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
