k: int = ...


def init():
    state.head = genesis


def mining():
    votes = children(state.head)
    if len(votes) >= k:
        return {votes.pop() for _ in range(k)}
    return {state.head}


def update(block):
    if is_vote(block):
        block = parents(block).pop()

    if height(block) > height(state.head):
        state.head = block

    elif height(block) == height(state.head):
        if len(children(block)) > len(children(state.head)):
            state.head = block


def is_vote(block):
    return len(parents(block)) == 1


def height(block):
    return len(history_of(block))


def history_of(block):
    if block == genesis:
        return [genesis]
    elif is_vote(block):
        return history_of(parents(block).pop())
    else:
        return history_of(parents(block).pop()) + [block]


def history():
    return history_of(state.head)


def progress(block):
    return k + 1


def coinbase(block):
    return [(miner_of(b), 1) for b in {block} | parents(block)]
