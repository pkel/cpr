from typing import Optional

from .protocol import Block, Protocol, Reward, View


class EthereumWhitepaper(Protocol):
    def __init__(self, *args, horizon=7):
        assert isinstance(horizon, int)
        assert horizon >= 0
        if horizon < 2:
            print("WARNING: Ethereum uncles are only feasible if horizon >= 2")

        self.horizon = horizon

    def __repr__(self):
        return f"EthereumWhitepaper(horizon={self.horizon})"

    @property
    def name(self):
        return f"ethereum-wp-{self.horizon}"

    def parent_and_uncles(self, v: View, b: Block):
        prio = [(v.height(x), x) for x in v.parents(b)]
        if len(prio) == 0:
            return None, set()
        parents = [p for (_, p) in sorted(prio)]
        return parents[-1], set(parents[:-1])

    def head(self, v: View, b: Block) -> Block:
        leaves = [x for x in v.descendants(b) | {b} if len(v.children(x)) == 0]
        prio = [(v.height(x), x) for x in leaves]
        leaves = [x for (_, x) in sorted(prio)]
        return leaves[-1]

    def uncle_candidates(self, v: View, head: Block) -> set[Block]:
        hist = [head]
        for _ in range(self.horizon):
            p = self.predecessor(v, hist[-1])
            if p is not None:
                hist.append(p)
            else:
                break

        uncle_candidates = set()
        # intentionally skip first two to make parent_and_uncles non-ambiguous
        for a in hist[2:]:
            for c in v.children(a):
                if self.predecessor(v, c) != a:
                    # child c actually has a different parent, maybe a fork
                    # See Example (1) at end of file.
                    continue
                if c not in hist:
                    uncle_candidates.add(c)

        assert all([v.height(u) < v.height(head) for u in uncle_candidates])

        return uncle_candidates

    def mining(self, v: View, b: Block) -> set[Block]:
        head = self.head(v, b)

        return {head} | self.uncle_candidates(v, head)

    def predecessor(self, v: View, b: Block) -> Optional[Block]:
        parent, _ = self.parent_and_uncles(v, b)
        return parent

    def preference(self, v: View, *args, old: Block, new: Block) -> Block:
        # preferred block is lagging self.horizon behind tip of chain (head)

        head = self.head(v, old)
        if v.height(new) > v.height(head):
            head = new

        pref = head
        for _ in range(self.horizon):
            p = self.predecessor(v, pref)
            if p is not None:
                pref = p
            else:
                break

        return pref

    def progress(self, v: View, b: Block) -> float:
        return v.height(b)

    def reward(self, v: View, b: Block) -> list[Reward]:
        assert len(v.parents(b)) > 0, "genesis reward not defined"

        rew = [Reward(v.miner(b), 1)]
        _, uncles = self.parent_and_uncles(v, b)
        for u in uncles:
            rew.append(Reward(v.miner(u), 1))
        return rew


class EthereumByzantium(EthereumWhitepaper):
    def __repr__(self):
        return f"EthereumByzantium(horizon={self.horizon})"

    @property
    def name(self):
        return f"ethereum-bz-{self.horizon}"

    def mining(self, v: View, b: Block) -> set[Block]:
        head = self.head(v, b)

        uncles = self.uncle_candidates(v, head)
        while len(uncles) > 2:
            uncles.pop()

        return {head} | set(uncles)

    def preference(self, v: View, *args, old: Block, new: Block) -> Block:
        # preferred block is lagging self.horizon behind tip of chain (head)

        head = self.head(v, old)
        if len(v.ancestors(new)) > len(v.ancestors(head)):
            head = new

        pref = head
        for _ in range(self.horizon):
            p = self.predecessor(v, pref)
            if p is not None:
                pref = p
            else:
                break

        return pref

    def progress(self, v: View, b: Block) -> float:
        return len(v.ancestors(b))

    def reward(self, v: View, b: Block) -> list[Reward]:
        assert len(v.parents(b)) > 0, "genesis reward not defined"

        _, uncles = self.parent_and_uncles(v, b)
        rew = [Reward(v.miner(b), 1 + 0.03125 * len(uncles))]
        h = v.height(b)
        max_d = self.horizon + 1  # TODO separate protocol parameter?
        for u in uncles:
            d = h - v.height(u)
            rew.append(Reward(v.miner(u), (max_d - d) / max_d))
        return rew


# Example (1)
# -----------
# head: 6
# hist: [6, 3, 2]
# child of hist member that is not a valid uncle: 5
# +------------+     +------------+     +------------+     +------------+
# | 5: I/K/F/3 | --> | 4: I/K/F/2 | --> | 1: I/K/F/1 | --> | 0: C/K/R/0 |
# +------------+     +------------+     +------------+     +------------+
#   |                                                        ^
#   +-------------------------------------+                  |
#                                         v                  |
# +------------+     +------------+     +------------+       |
# | 6: C/K/R/3 | --> | 3: C/K/R/2 | --> | 2: P/P/R/1 | ------+
# +------------+     +------------+     +------------+
