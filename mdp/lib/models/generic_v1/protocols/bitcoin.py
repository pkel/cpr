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

    def history(self):
        def history_of(block):
            if block == self.genesis:
                return [self.genesis]
            else:
                return history_of(self.parents(block).pop()) + [block]

        return history_of(self.state.head)

    def progress(self, block):
        return 1

    def coinbase(self, block):
        return [(self.miner_of(block), 1)]


class Protocol(Protocol0):
    def relabel_state(self, new_ids):
        self.state.head = new_ids[self.state.head]

    def color_block(self, block):
        if self.state.head == block:
            return 1
        else:
            return 0

    def collect_garbage(self):
        return {self.state.head}
