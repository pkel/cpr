from ..listings import listing
from .interface import Protocol as Interface


@listing("ghostdag")
class Protocol0(Interface):
    k: int = ...

    def mining(self):
        return self.tips(self.G)

    def history_of(self, G):  # eprint.iacr.org/2018/104.pdf ; Alg. 1
        if len(G) == 1:
            return ({self.genesis}, [self.genesis])

        blue, hist = dict(), dict()
        for b in self.tips(G):
            blue[b], hist[b] = self.history_of(self.past(G, b))
        b_max = max((len(blue[b]), hash(b), b) for b in self.tips(G))[2]
        blue = blue[b_max] | {b_max}
        hist = hist[b_max] + [b_max]

        for b in self.topological_order(self.anticone(G, b_max)):
            if self.is_k_cluster(G, blue | {b}):
                blue = blue | {b}
                hist = hist + [b]  # only blue blocks get a reward

        return (blue, hist)

    def is_k_cluster(self, G, S):
        return all(len(self.anticone(G, b) & S) <= self.k for b in S)

    def history(self):
        _blue, history = self.history_of(self.G)
        return history

    # init() and fn update() do nothing
    # progress() and coinbase() are like in Bitcoin


@listing("ghostdag_util")
class Protocol1(Protocol0):
    k = 3

    # omitted, boring helper functions

    def tips(self, subgraph):
        return {b for b in subgraph if len(self.children(b) & subgraph) == 0}

    def past_or_future(self, relation, subgraph, block):
        acc = set()
        stack = set(relation(block)) & subgraph
        while len(stack) > 0:
            b = stack.pop()
            if b not in acc:
                acc.add(b)
                for p in set(relation(b)) & subgraph:
                    stack.add(p)
        return acc

    def past(self, subgraph, block):
        return self.past_or_future(self.parents, subgraph, block)

    def future(self, subgraph, block):
        return self.past_or_future(self.children, subgraph, block)

    def anticone(self, subgraph, block):
        return (
            subgraph
            - {block}
            - self.past(subgraph, block)
            - self.future(subgraph, block)
        )

    # omitted, boring protocol spec functions

    def init(self):
        pass

    def update(self, block):
        pass

    def progress(self, block):
        return 1

    def coinbase(self, block):
        return [(self.miner_of(block), 1)]


class Protocol(Protocol1):
    def __init__(self, *, k: int):
        super().__init__()
        self.k = k

    def relabel_state(self, new_ids):
        pass

    def collect_garbage(self):
        return self.tips(self.G)
