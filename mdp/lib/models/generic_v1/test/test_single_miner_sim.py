from ..protocols import Bitcoin, Ghostdag, Parallel
from ..sim import SingleMinerSim


def single_miner_sim(protocol, **kwargs):
    s = SingleMinerSim(protocol, **kwargs)
    s.sim(100)


def test_bitcoin():
    single_miner_sim(Bitcoin)


def test_ghostdag_7():
    single_miner_sim(Ghostdag, k=7)


def test_parallel_7():
    single_miner_sim(Parallel, k=7)
