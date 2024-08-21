### MINER'S STATE


class State:
    def __setattr__(self, name, value):
        self.__dict__[name] = value


# State() objects allow to create attributes on first assign

state = [State()]
# each miner has a state object; we have one miner

# blocks are numbers 0, 1, ...
genesis = 0

# graphs are a subset of these numbers
G = {genesis}

# parent relation is stored as adjacency list
_parents = {genesis: []}
_children = {genesis: set()}


def parents(b):
    return _parents[b]


def children(b):
    return _children[b]


# there is only one miner, 0
def miner_of(b):
    return 0


def topological_order(subgraph):
    return sorted(list(subgraph))


## PROTOCOL SPEC IMPORT

import importlib, os, sys

sys.path.append(os.getcwd())


def import_protocol(name):
    def patch_globals(module):
        module.genesis = genesis
        module.parents = parents
        module.children = children
        module.miner_of = miner_of
        module.topological_order = topological_order
        module.G = G

    protocol = importlib.import_module(f"listings.{name}")
    patch_globals(protocol)

    try:
        util = importlib.import_module(f"listings.{name}_util")
        patch_globals(util)
        for u in dir(util):
            if u.startswith("_"):
                pass
            else:
                setattr(protocol, u, getattr(util, u))
    except ModuleNotFoundError:
        pass

    return protocol


protocol = import_protocol(os.getenv("PROTOCOL", "bitcoin"))

## SIMULATION


def step():
    # the single miner mines a block
    m = 0
    # (in multi-miner network, we would sample one of them)

    b = len(G)
    _parents[b] = protocol.mining(state[0])
    _children[b] = set()
    for p in _parents[b]:
        _children[p].add(b)
    G.add(b)

    # and separately learns about the new block
    protocol.update(state[0], b)
    # (multi-miner net: update the other nodes, maybe after some time delay)


def reward_and_progress(miner):
    history = protocol.history(state[miner])
    rew = {m: 0 for m in range(len(state))}
    prg = 0
    for b in history:
        # accumulate rewards
        for miner, amount in protocol.coinbase(b):
            rew[miner] += amount
        # accumulate progress
        prg += protocol.progress(b)
    return (rew, prg)


# run simulation up to given progress, looking at miner=0
def sim(max_progress):
    i = 0
    prg = 0
    while prg < max_progress:
        step()
        rew, prg = reward_and_progress(0)

        i += 1

    return rew[0]  # reward of miner 0


protocol.init(state[0])
print("simulated up to progress", sim(100))
