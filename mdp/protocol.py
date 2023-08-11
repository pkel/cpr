from dataclasses import dataclass
from typing import Optional


class Block:
    def __init__(self, *args, parents: list["Block"]):
        if parents == []:
            # genesis block defines DAG
            self.__dag__ = id(self)
        else:
            # different DAGs cannot be joined
            self.__dag__ = parents[0].__dag__
            for p in parents[1:]:
                if p.__dag__ != self.__dag__:
                    raise ValueError("parents must be of the same DAG")

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
    def genesis(self) -> Template:
        raise NotImplementedError

    def unsafe_validity(self, b: Block) -> bool:
        raise NotImplementedError

    def validity(self, b: Block) -> bool:
        try:
            return self.unsafe_validity(b)
        except AssertionError:
            return False

    def progress(self, b: Block) -> float:
        raise NotImplementedError

    def predecessor(self, b: Block) -> Optional[Block]:
        raise NotImplementedError

    def reward(self, b: Block) -> list[Reward]:
        raise NotImplementedError

    def preference(self, old: Block, new: Block) -> Block:
        raise NotImplementedError

    def mining(self, b: Block) -> Template:
        raise NotImplementedError
