from ..listings import listing
from .ethereum import Protocol as Ethereum


@listing("byzantium")
class Protocol(Ethereum):
    def mining(self):
        uncles = self.available_uncles()
        while len(uncles) > 2:
            uncles.pop()
        return {self.state.head} | uncles

    def update(self, block):
        prg_new = [self.progress(b) for b in self.history_of(block)]
        prg_old = [self.progress(b) for b in self.history_of(self.state.head)]
        if prg_new > prg_old:
            self.state.head = block

    def progress(self, block):
        _, uncles = self.parent_and_uncles(block)
        return 1 + len(uncles)

    def coinbase(self, block):
        _, uncles = self.parent_and_uncles(block)
        lst = [(self.miner_of(block), 1 + 0.3125 * len(uncles))]
        h = self.height(block)
        max_d = self.horizon + 1
        for u in uncles:
            d = h - self.height(u)
            lst.append((self.miner_of(u), (max_d - d) / max_d))
        return lst
