### MINER'S STATE


class State:
    def __setattr__(self, name, value):
        self.__dict__[name] = value


# State() objects allow to create attributes on first assign

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


def load_protocol(name, state):
    def patch_globals(module):
        module.state = state
        module.genesis = genesis
        module.parents = parents
        module.children = children
        module.miner_of = miner_of
        module.topological_order = topological_order
        module.G = G

    miner = importlib.import_module(f"listings.{name}")
    patch_globals(miner)

    try:
        util = importlib.import_module(f"listings.{name}_util")
        patch_globals(util)
        for u in dir(util):
            if u.startswith("_"):
                pass
            else:
                setattr(miner, u, getattr(util, u))
    except ModuleNotFoundError:
        pass

    miner.init()

    return miner


miner = load_protocol(os.getenv("PROTOCOL", "bitcoin"), State())

## SIMULATION


def step():
    b = len(G)  # new block
    _parents[b] = miner.mining()
    _children[b] = set()
    for p in _parents[b]:
        _children[p].add(b)
    G.add(b)

    # and separately learns about the new block
    miner.update(b)
    # (multi-miner net: update the other nodes, maybe after some time delay)


def reward_and_progress():
    history = miner.history()
    rew = 0
    prg = 0
    for b in history:
        # accumulate rewards
        for _, amount in miner.coinbase(b):
            rew += amount
        # accumulate progress
        prg += miner.progress(b)
    return (rew, prg)


# run simulation up to given progress, looking at miner=0
def sim(max_progress):
    i = 0
    prg = 0
    while prg < max_progress:
        step()
        rew, prg = reward_and_progress()

        i += 1

    return rew  # reward of miner 0


print("simulated up to progress", sim(100))
