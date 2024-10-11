def init():
    state.head = genesis


def mining():
    return {state.head}


def update(block):
    if height(block) > height(state.head):
        state.head = block


def height(block):
    return len(history_of(block))


def history_of(block):
    if block == genesis:
        return [genesis]
    else:
        return history_of(parents(block).pop()) + [block]


def history():
    return history_of(state.head)


def progress(block):
    return 1


def coinbase(block):
    return [(miner_of(block), 1)]
