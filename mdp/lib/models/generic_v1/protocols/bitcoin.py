from ..listings import listing
from .interface import Protocol as Interface


@listing("bitcoin")
class Protocol0(Interface):
    def init(self):
        self.state.head = self.genesis

    def mining(self):
        return {self.state.head}

    def update(self, block):
        if self.height(block) > self.height(self.state.head):
            self.state.head = block

    def height(self, block):
        return len(self.history_of(block))

    def history_of(self, block):
        if block == self.genesis:
            return [self.genesis]
        else:
            return self.history_of(self.parents(block).pop()) + [block]

    def history(self):
        return self.history_of(self.state.head)

    def progress(self, block):
        return 1

    def coinbase(self, block):
        return [(self.miner_of(block), 1)]


class Protocol(Protocol0):
    def relabel_state(self, new_ids):
        self.state.head = new_ids[self.state.head]

    def collect_garbage(self):
        return {self.state.head}
