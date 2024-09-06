from ..model import DAG, DynObj
import os
import importlib


def import_spec(name):
    path = os.path.abspath(__file__)
    path = os.path.dirname(path)
    path = os.path.dirname(path)
    path = path + "/listings/" + name + ".py"

    spec = importlib.util.spec_from_file_location(f"listings.{name}", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def load_protocol(name, dag, state):
    def patch_globals(module):
        module.state = state
        module.genesis = dag.genesis
        module.parents = dag.parents
        module.children = dag.children
        module.miner_of = dag.miner_of
        module.topological_order = dag.topological_order
        module.G = {dag.genesis}

    miner = import_spec(name)
    patch_globals(miner)

    try:
        util = import_spec(name + "_util")
        patch_globals(util)
        for u in dir(util):
            if u.startswith("_"):
                pass
            else:
                setattr(miner, u, getattr(util, u))
    except FileNotFoundError:
        pass

    miner.init()

    return miner


def reward_and_progress(miner):
    history = miner.history()
    rew, prg = 0, 0
    for b in history:
        for _, amount in miner.coinbase(b):
            rew += amount
        prg += miner.progress(b)
    return (rew, prg)


def sim(max_progress, protocol):
    dag = DAG()
    state = DynObj()
    miner = load_protocol(protocol, dag, state)

    # run simulation up to given progress, looking at miner=0
    i = 0
    prg = 0
    while prg < max_progress:
        parents = miner.mining()
        b = dag.append(parents, 0)
        miner.update(b)
        miner.G.add(b)

        rew, prg = reward_and_progress(miner)

        i += 1


def test_bitcoin():
    sim(100, "bitcoin")


def test_ghostdag():
    sim(100, "ghostdag")
