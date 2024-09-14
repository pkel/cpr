def init(state):
    state.head = genesis


def mining(state):
    return [state.head]


def update(state, block):
    if height(block) > height(state.head):
        state.head = block


def height(block):
    return len(history_of(block))


def history_of(block):
    if block == genesis:
        return [genesis]
    else:
        return history_of(parents(block)[0]) + [block]


def history(state):
    return history_of(state.head)


def progress(block):
    return 1


def coinbase(block):
    return [(miner_of(block), 1)]
