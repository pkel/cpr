from ..listings import generate_listing
from ..model import DAG, DynObj
import types


def load_listing(name):
    code = generate_listing(name)

    module_name = f"listings.{name}"
    module = types.ModuleType(module_name)
    exec(code, module.__dict__)

    return module


def load_protocol(name, dag, state):
    def patch_globals(module):
        module.state = state
        module.genesis = dag.genesis
        module.parents = dag.parents
        module.children = dag.children
        module.miner_of = dag.miner_of
        module.height = dag.height
        module.topological_order = dag.topological_order
        module.G = {dag.genesis}

    miner = load_listing(name)
    patch_globals(miner)

    try:
        util = load_listing(name + "_util")
        patch_globals(util)
        for u in dir(util):
            if u.startswith("_"):
                pass
            else:
                setattr(miner, u, getattr(util, u))
    except KeyError:
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

        if i > max_progress * 10:
            raise RuntimeError("diminishing progress")


def test_bitcoin():
    sim(100, "bitcoin")


def test_ethereum():
    sim(100, "ethereum")


def test_ghostdag():
    sim(100, "ghostdag")


def test_parallel():
    sim(100, "parallel")
