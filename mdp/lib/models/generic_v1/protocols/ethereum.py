from ..listings import listing
from .interface import Protocol as Interface


@listing("ethereum")
class Protocol0(Interface):
    h: int = 3

    def init(self):
        self.state.head = self.genesis

    def mining(self):
        return {self.state.head} | self.available_uncles()

    def available_uncles(self):
        hist = self.history_of(self.state.head)
        allowed_parents = set(hist[-self.h - 1 : -2])
        uncles = set()
        leaves = {b for b in self.G if len(self.children(b)) == 0}
        for b in leaves:
            p, _ = self.parent_and_uncles(b)
            if p in allowed_parents:
                uncles.add(b)
        assert all(self.height(u) < self.height(self.state.head) for u in uncles)
        return uncles

    def update(self, block):
        if self.height(block) > self.height(self.state.head):
            self.state.head = block

    def history_of(self, block):
        if block == self.genesis:
            return [self.genesis]
        else:
            parent, _ = self.parent_and_uncles(block)
            return self.history_of(parent) + [block]

    def parent_and_uncles(self, block):
        parents_with_prio = [(-self.height(p), p) for p in self.parents(block)]
        prioritized_parents = [p for _, p in sorted(parents_with_prio)]
        if len(prioritized_parents) > 0:
            return prioritized_parents[0], set(prioritized_parents[1:])
        else:
            return None, set()

    def history(self):
        return self.history_of(self.state.head)

    def progress(self, block):
        return 1

    def coinbase(self, block):
        _, uncles = self.parent_and_uncles(block)
        return [(self.miner_of(b), 1) for b in {block} | uncles]


class Protocol(Protocol0):
    def __init__(self, h: int = 7):
        if h < 2:
            print("WARNING: Ethereum uncles are feasible for horizon h >= 2 only")

        self.h = h

    def relabel_state(self, new_ids):
        self.state.head = new_ids[self.state.head]

    def color_block(self, block):
        if self.state.head == block:
            return 1
        else:
            return 0

    def collect_garbage(self):
        return {self.state.head} | self.available_uncles()
