from dataclasses import dataclass
from typing import Optional, TypeVar

Block = TypeVar("Block")
Miner = TypeVar("Miner")


class View:
    def parents(self, b: Block) -> set[Block]:
        raise NotImplementedError

    def children(self, b: Block) -> set[Block]:
        raise NotImplementedError

    def miner(self, b: Block) -> Miner:
        raise NotImplementedError

    def height(self, b: Block) -> int:
        raise NotImplementedError

    def ancestors(self, b: Block) -> set[Block]:
        acc = set()
        todo = self.parents(b).copy()
        while len(todo) > 0:
            b = todo.pop()
            if b not in acc:
                acc.add(b)
                todo |= self.parents(b)
        return acc

    def descendants(self, b: Block) -> set[Block]:
        acc = set()
        todo = self.children(b).copy()
        while len(todo) > 0:
            b = todo.pop()
            if b not in acc:
                acc.add(b)
                todo |= self.children(b)
        return acc


@dataclass(frozen=True)
class Reward:
    miner: Miner
    amount: float


class Protocol:
    """
    Protocol Specification API.
    """

    @property
    def name(self) -> str:
        raise NotImplementedError

    def mining(self, v: View, b: Block) -> set[Block]:
        """
        Participants extend the block DAG according to this rule. Recall that
        all blocks require a proof-of-work. Returns the set of blocks to be
        confirmed by the new block.
        """
        raise NotImplementedError

    def preference(self, v: View, *args, old: Block, new: Block) -> Block:
        """
        Participants update their preferred block according to this rule.
        """
        raise NotImplementedError

    def predecessor(self, v: View, b: Block) -> Optional[Block]:
        """
        Define block history. We're evaluating total order broadcast protocols.
        Each participant has a preferred block. The history of ordered messages
        is can be derived by calling this function recursively.

        This function is used to recognize and determine the length of history
        rewrites. It's also used to determine the blocks that pay out rewards.
        """
        raise NotImplementedError

    def progress(self, v: View, b: Block) -> float:
        """
        Define DAA target. The DAA should ensure that progress grows at a
        constant rate.
        """
        raise NotImplementedError

    def reward(self, v: View, b: Block) -> list[Reward]:
        """
        Define the coinbase transactions of a block.
        """
        raise NotImplementedError
