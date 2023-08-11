from dataclasses import dataclass
from typing import Optional

import protocol
from protocol import Protocol, Reward


@dataclass
class Template(protocol.Template):
    height: int
    kind: str


class Block(Template, protocol.Block):
    def __init__(self, *args, parents: list["Block"], **kwargs):
        Template.__init__(self, parents=parents, **kwargs)
        protocol.Block.__init(self, parents=parents)


class Parallel(Protocol):
    def __init__(self, *args, k: int):
        if k < 2:
            raise ValueError("k must be greater than 1")
        self.k = k

    def genesis(self) -> Template:
        return Template(height=0, kind="seq")

    def unsafe_validity(self, b: Block) -> bool:
        if b.kind == "seq":
            assert len(b.parents) == self.k - 1
            pseq = b.parents[0].parents[0]
            for p in b.parents[1:]:
                assert p.parents[0] == pseq, "parents must confirm single block"
            return b.height == pseq.height + 1
        elif b.kind == "par":
            assert len(b.parents) == 1
            return b.height == b.parents[0].height
        else:
            assert False, f"kind must be 'seq' or 'par', not '{b.kind}'"

    def progress(self, b: Block) -> float:
        return b.height * self.k

    def predecessor(self, b: Block) -> Optional[Block]:
        assert b.kind == "seq"
        if len(b.parents) > 0:
            return b.parents[0].parents[0]
        else:
            return None

    def reward(self, b: Block) -> list[Reward]:
        assert b.kind == "seq"
        rewards = [Reward(b.miner, 1)]
        for p in b.parents:
            rewards.append(Reward(p.miner, 1))
        return rewards

    def preference(self, old: Block, new: Block) -> Block:
        if new.kind != "seq":
            new = new.parents[0]

        if new.height > old.height:
            return new
        elif new.height < old.height:
            return old
        else:
            if len(new.children) > len(old.children):
                return new
            else:
                return old

    def mining(self, b: Block) -> Template:
        if len(b.children) >= self.k - 1:
            # TODO select k-1 par-blocks, preferring own over foreign
            confirm = ...
            return Template(kind="seq", height=b.heigth + 1, parents=confirm)
        else:
            return Template(kind="par", height=b.height, parents=[b])
