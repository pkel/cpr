from typing import Optional

from protocol import Block, Protocol, Reward, View


class Parallel(Protocol):
    def __init__(self, *args, k: int):
        if k < 2:
            raise ValueError("k must be greater 1")
        self.k = k

    @property
    def name(self):
        return f"parallel-{self.k}"

    def mining(self, v: View, b: Block) -> set[Block]:
        children = v.children(b)
        if len(children) >= self.k:
            return {children.pop() for _ in range(self.k)}
        else:
            return {b}

    def is_vote(self, v: View, b: Block):
        return len(v.parents(b)) == 1

    def predecessor(self, v: View, b: Block) -> Optional[Block]:
        assert not self.is_vote(v, b)
        votes = v.parents(b)
        if len(votes) == 0:
            return None
        else:
            assert len(votes) == self.k, f"{len(votes)} != {self.k}"
            vote = votes.pop()
            return v.parents(vote).pop()

    def height(self, v: View, b: Block) -> int:
        h = 0
        p = self.predecessor(v, b)
        while p is not None:
            h += 1
            p = self.predecessor(v, p)
        return h

    def preference(self, v: View, *args, old: Block, new: Block) -> Block:
        if self.is_vote(v, new):
            new = v.parents(new).pop()
        h_new = self.height(v, new)
        h_old = self.height(v, old)
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
        assert not self.is_vote(v, b)
        votes = v.parents(b)
        if len(votes) == 0:  # genesis
            return []
        else:
            assert len(votes) == self.k, f"{len(votes)} != {self.k}"
            return [Reward(v.miner(x), 1) for x in list(votes) + [b]]
