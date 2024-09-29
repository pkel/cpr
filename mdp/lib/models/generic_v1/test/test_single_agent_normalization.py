from ..model import SingleAgentImp
from ..protocols import Bitcoin


def test_relabel():
    a = SingleAgentImp(Bitcoin)
    a.relabel([0])

    a.dag.append([0], 0)
    a.relabel([0, 1])
    a.attacker.deliver(1)
    a.defender.deliver(1)
    a.relabel([1, 0])


def test_normalize():
    a = SingleAgentImp(Bitcoin)

    b = a.normalize()
    c = a.normalize()

    d = dict()
    d[b] = "b"
    d[c] = "c"

    assert b == c
    assert d[b] == "c"
