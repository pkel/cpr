from ..protocols import *
from ..sim import DiscreteEventSim, NetworkSim


def test_discrete_event_sim():
    s = DiscreteEventSim()
    out = []

    # schedule stuff in the (virtual) future
    s.delay(2, out.append, 2)
    s.delay(0, out.append, 0)
    s.delay(9, out.append, 9)
    s.delay(3, out.append, 3)

    # run the simulation up to a certain point
    s.loop(lambda x: x.clock >= 3)

    # check output
    assert out == [0, 2, 3]


from numpy import random


def sim_3_ethereum(upto_progress, *args, **kwargs):
    """
    Three nodes, Ethereum-like network conditions
    """
    s = NetworkSim(
        *args,
        **kwargs,
        n_miners=3,
        mining_delay=lambda: random.exponential(13),
        select_miner=lambda: random.randint(0, 3),
        message_delay=lambda: random.uniform(2),
    )
    s.sim(upto_progress)


def test_bitcoin():
    sim_3_ethereum(100, Bitcoin)


def test_ghostdag_3():
    sim_3_ethereum(50, Ghostdag, k=3)
