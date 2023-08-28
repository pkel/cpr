from typing import Optional

from protocol import Block, Protocol, Reward, View


class Bitcoin(Protocol):
    @property
    def name(self):
        return "bitcoin"

    def mining(self, v: View, b: Block) -> set[Block]:
        return {b}

    def predecessor(self, v: View, b: Block) -> Optional[Block]:
        parents = v.parents(b)
        if len(parents) == 1:
            return list(parents)[0]
        else:
            return None

    def height(self, v: View, b: Block) -> int:
        h = 0
        p = self.predecessor(v, b)
        while p is not None:
            h += 1
            p = self.predecessor(v, p)
        return h

    def preference(self, v: View, *args, old: Block, new: Block) -> Block:
        if v.height(new) > v.height(old):
            return new
        else:
            return old

    def progress(self, v: View, b: Block) -> float:
        return self.height(v, b)

    def reward(self, v: View, b: Block) -> list[Reward]:
        return [Reward(v.miner(b), 1)]
