from ..listings import listing
from .interface import Protocol as Interface


@listing("parallel")
class Protocol0(Interface):
    k: int = ...  # protocol parameter

    def init(self):
        self.state.head = self.genesis

    def mining(self):
        votes = self.children(self.state.head)
        if len(votes) >= self.k:
            return {votes.pop() for _ in range(self.k)}
        return {self.state.head}

    def update(self, block):
        if self.is_vote(block):
            block = self.parents(block).pop()
        assert not self.is_vote(block)
        if self.height(block) > self.height(self.state.head):
            self.state.head = block
        elif self.height(block) == self.height(self.state.head):
            if len(self.children(block)) > len(self.children(self.state.head)):
                self.state.head = block

    def is_vote(self, block):
        return len(self.parents(block)) == 1

    def height(self, block):
        return len(self.history_of(block))

    def history_of(self, block):
        if block == self.genesis:
            return [self.genesis]
        elif self.is_vote(block):
            return self.history_of(self.parents(block).pop())
        else:
            return self.history_of(self.parents(block).pop()) + [block]

    def history(self):
        return self.history_of(self.state.head)

    def progress(self, block):
        assert not self.is_vote(block)

        return self.k + 1

    def coinbase(self, block):
        assert not self.is_vote(block)

        return [(self.miner_of(b), 1) for b in {block} | self.parents(block)]


@listing("parallel_util")
class Protocol1(Protocol0):
    k = 3


class Protocol(Protocol1):
    def __init__(self, *, k):
        assert k >= 2  # required to distinguish votes from blocks in self.is_vote()

        self.k = k

    def relabel_state(self, new_ids):
        self.state.head = new_ids[self.state.head]

    def collect_garbage(self):
        return {self.state.head} | self.children(self.state.head)
