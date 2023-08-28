from typing import Optional

from protocol import Block, Protocol, Reward, View


class EthereumWhitepaper(Protocol):
    def __init__(self, *args, horizon=7):
        assert isinstance(horizon, int)
        assert horizon >= 0

        self.horizon = horizon

    def __repr__(self):
        return f"{EthereumWhitepaper(horizon = {self.horizon})}"

    @property
    def name(self):
        return f"ethereum-wp-{self.horizon}"

    def parent_and_uncles(self, v: View, b: Block):
        prio = [(v.height(x), x) for x in v.parents(b)]
        if len(prio) == 0:
            return None, set()
        parents = [p for (_, p) in sorted(prio)]
        return parents[-1], set(parents[:-1])

    def head_and_leaves(self, v: View, b: Block) -> Block:
        leaves = [x for x in v.descendants(b) | {b} if len(v.children(x)) == 0]
        prio = [(v.height(x), x) for x in leaves]
        leaves = [x for (_, x) in sorted(prio)]
        return leaves[-1], set(leaves[:-1])

    def mining(self, v: View, b: Block) -> set[Block]:
        head, leaves = self.head_and_leaves(v, b)

        height_head = v.height(head)
        uncles = {x for x in leaves if v.height(x) < height_head}

        return {head} | uncles

    def predecessor(self, v: View, b: Block) -> Optional[Block]:
        parent, _ = self.parent_and_uncles(v, b)
        return parent

    def preference(self, v: View, *args, old: Block, new: Block) -> Block:
        # preferred block is lagging self.horizon behind tip of chain (head)

        head, _ = self.head_and_leaves(v, old)
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
        rew = [Reward(v.miner(b), 1)]
        _, uncles = self.parent_and_uncles(v, b)
        for u in uncles:
            rew.append(Reward(v.miner(u, 1)))
        return rew


class EthereumByzantium(EthereumWhitepaper):
    def __repr__(self):
        return f"{EthereumByzantium(horizon = {self.horizon})}"

    @property
    def name(self):
        return f"ethereum-bz-{self.horizon}"

    def mining(self, v: View, b: Block) -> set[Block]:
        head, leaves = self.head_and_leaves(v, b)

        height_head = v.height(head)
        uncles = [x for x in leaves if v.height(x) < height_head]
        if len(uncles) > 2:
            uncles = uncles[0:2]

        return {head} | set(uncles)

    def preference(self, v: View, *args, old: Block, new: Block) -> Block:
        # preferred block is lagging self.horizon behind tip of chain (head)

        head, _ = self.head_and_leaves(v, old)
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
        _, uncles = self.parent_and_uncles(v, b)
        rew = [Reward(v.miner(b), 1 + 0.3125 * len(uncles))]
        h = v.height(b)
        max_d = self.horizon + 1  # TODO separate protocol parameter?
        for u in uncles:
            d = h - v.height(u)
            rew.append(Reward(v.miner(u, (max_d - d) / max_d)))
        return rew
