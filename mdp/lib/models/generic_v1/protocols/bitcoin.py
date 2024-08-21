# The following class(es) will go straight into the paper; references to self.*
# will be replaced with global variables.


# goes to listings/%
class Listing:
    def init(self, state):
        state.head = self.genesis

    def mining(self, state):
        return [state.head]

    def update(self, state, block):
        if self.height(block) > self.height(state.head):
            state.head = block

    def height(self, block):
        return len(self.history_of(block))

    def history_of(self, block):
        if block == self.genesis:
            return [self.genesis]
        else:
            return self.history_of(self.parents(block)[0]) + [block]

    def history(self, state):
        return history_of(state.head)

    def progress(self, _block):
        return 1

    def coinbase(self, block):
        return [(self.miner_of(block), 1)]


def Protocol(Listing):
    pass
