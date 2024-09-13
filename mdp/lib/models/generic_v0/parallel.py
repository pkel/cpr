from typing import Optional

from .protocol import Block, Protocol, Reward, View


class Parallel(Protocol):
    def __init__(self, *args, k: int):
        if k < 2:
            raise ValueError("k must be greater 1")
        self.k = k

    @property
    def name(self):
        return f"parallel-{self.k}"

    def __repr__(self):
        return f"Parallel(k={self.k})"

    def mining(self, v: View, b: Block) -> set[Block]:
        children = v.children(b)
        if len(children) >= self.k:
            return {children.pop() for _ in range(self.k)}
        else:
            return {b}

    def parent_and_votes(self, v: View, b: Block):
        votes = v.parents(b)
        if len(votes) == 0:
            return None, None  # genesis

        assert len(votes) == self.k

        grandparents = set()
        for vote in votes:
            grandparents |= v.parents(vote)

        assert len(grandparents) == 1

        return grandparents.pop(), votes

    def is_block(self, v: View, b: Block):
        parents = v.parents(b)
        if len(parents) == 1:
            return False
        elif len(parents) == 0:
            # genesis / root
            return True
        elif len(parents) == self.k:
            # block with history
            return True
        else:
            assert False, f"{len(parents)}, {self.k}"

    def predecessor(self, v: View, b: Block) -> Optional[Block]:
        assert self.is_block(v, b)
        parent, votes = self.parent_and_votes(v, b)
        return parent

    def preference(self, v: View, *args, old: Block, new: Block) -> Block:
        if not self.is_block(v, new):
            new = v.parents(new).pop()

        assert self.is_block(v, new)
        assert self.is_block(v, old)

        h_new = v.height(new)
        h_old = v.height(old)
        if h_new > h_old:
            return new
        elif h_new < h_old:
            return old
        else:  # h_new == h_old
            if len(v.children(new)) > len(v.children(old)):
                return new
            else:
                return old

    def progress(self, v: View, b: Block) -> float:
        return len(v.ancestors(b))

    def reward(self, v: View, b: Block) -> list[Reward]:
        assert self.is_block(v, b)
        parent, votes = self.parent_and_votes(v, b)

        assert parent is not None, "genesis reward not defined"

        assert len(votes) == self.k, f"{len(votes)} != {self.k}"
        return [Reward(v.miner(x), 1) for x in list(votes) + [b]]
