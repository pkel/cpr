from dataclasses import dataclass
from typing import Optional

import protocol
from protocol import Protocol, Reward


@dataclass
class Template(protocol.Template):
    height: int


class Block(Template, protocol.Block):
    def __init__(self, *args, parents: list["Block"], **kwargs):
        Template.__init__(self, parents=parents, **kwargs)
        protocol.Block.__init(self, parents=parents)


class Bitcoin(Protocol):
    def genesis(self) -> Template:
        return Template(height=0)

    def unsafe_validity(self, b: Block) -> bool:
        assert len(b.parents) == 1
        return b.height == b.parents[0].height + 1

    def progress(self, b: Block) -> float:
        return b.height

    def predecessor(self, b: Block) -> Optional[Block]:
        return b.parents[0] if len(b.parents) > 0 else None

    def reward(self, b: Block) -> list[Reward]:
        return [Reward(b.miner, 1)]

    def preference(self, old: Block, new: Block) -> Block:
        return new if new.height > old.height else old

    def mining(self, b: Block) -> Template:
        return Template(height=b.height + 1)
