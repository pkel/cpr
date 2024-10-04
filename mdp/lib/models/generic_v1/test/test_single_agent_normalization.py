from ..model import SingleAgentImp
from ..protocols import Bitcoin


def test_relabel():
    a = SingleAgentImp(Bitcoin)
    _ = a.copy_and_relabel([0])

    a.dag.append([0], 0)
    _ = a.copy_and_relabel([0, 1])
    a.attacker.deliver(1)
    a.defender.deliver(1)
    _ = a.copy_and_relabel([1, 0])


def test_normalize():
    a = SingleAgentImp(Bitcoin)

    b = a.copy_and_normalize()
    c = a.copy_and_normalize()

    b.freeze()
    c.freeze()

    d = dict()
    d[b] = "b"
    d[c] = "c"

    assert b == c
    assert d[b] == "c"
