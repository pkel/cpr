from dataclasses import dataclass
from typing import Optional


class Block:
    @property
    def parents(self) -> list["Block"]:
        raise NotImplementedError

    @property
    def children(self) -> list["Block"]:
        raise NotImplementedError


@dataclass
class Template:
    parents: list[Block]


@dataclass
class Reward:
    miner: int
    amount: float


class Protocol:
    """
    Protocol Specification API.
    """

    def genesis(self) -> Template:
        """
        Defines first block of the block DAG.
        """
        raise NotImplementedError

    def unsafe_validity(self, b: Block) -> bool:
        """
        Defines validity blocks. All blocks have to pass this test. The sole
        exception to this rule is the genesis block defined above.

        All blocks implicitly require a proof-of-work.

        This function may raise AssertionError instead of returning False.
        """
        raise NotImplementedError

    def validity(self, b: Block) -> bool:
        """
        Wrapper around unsafe_validity which return False instead of raising
        AssertionError.
        """
        try:
            return self.unsafe_validity(b)
        except AssertionError:
            return False

    def progress(self, b: Block) -> float:
        """
        Define DAA target. The DAA should ensure that progress grows at a
        constant rate.
        """
        raise NotImplementedError

    def predecessor(self, b: Block) -> Optional[Block]:
        """
        Define block history. We're evaluating total order broadcast protocols.
        Each participant has a preferred block. The history of ordered messages
        is can be derived by calling this function recursively.

        This function is used to recognize and determine the length of history
        rewrites. It's also used to determine the blocks that pay out rewards.
        """
        raise NotImplementedError

    def reward(self, b: Block) -> list[Reward]:
        """
        Define the coinbase transactions of a block.
        """

        raise NotImplementedError

    def preference(self, old: Block, new: Block) -> Block:
        """
        Participants update their preferred block according to this rule.
        """
        raise NotImplementedError

    def mining(self, b: Block) -> Template:
        """
        Participants extend the block DAG according to this rule. Recall that
        all blocks require a proof-of-work.
        """
        raise NotImplementedError
