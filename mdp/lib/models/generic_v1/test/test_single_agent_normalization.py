from ..model import SingleAgentImp
from ..protocols import Bitcoin


def test_relabel():
    a = SingleAgentImp(Bitcoin)
    _ = a.copy_and_relabel([0])

    a.dag.append({0}, 0)
    _ = a.copy_and_relabel([0, 1])
    a.attacker.deliver(1)
    a.defender.deliver(1)
    _ = a.copy_and_relabel([0, 1])


def test_hash_and_eq_of_copy():
    a = SingleAgentImp(Bitcoin)
    b = a.copy()
    c = a.copy_and_normalize()

    a.freeze()
    b.freeze()
    c.freeze()

    assert a == b
    assert a == c

    d = dict()
    d[a] = "a"
    d[b] = "b"
    assert d[a] == "b"
    d[c] = "c"
    assert d[a] == "c"


def test_hash_and_eq_of_equivalant_dag():
    a = SingleAgentImp(Bitcoin)
    b = a.copy()

    a.dag.append({0}, 0)
    a.dag.append({1}, 0)
    a.dag.append({1}, 0)

    b.dag.append({0}, 0)
    b.dag.append({1}, 0)
    b.dag.append({1}, 0)

    a.freeze()
    b.freeze()

    assert a == b


def test_normalize_single_color():
    a = SingleAgentImp(Bitcoin)
    b = a.copy()

    a.dag.append({0}, 0)
    a.dag.append({1}, 0)
    a.dag.append({1}, 0)
    a.dag.append({2}, 0)

    b.dag.append({0}, 0)
    b.dag.append({1}, 0)
    b.dag.append({1}, 0)
    b.dag.append({3}, 0)

    an = a.copy_and_normalize()
    bn = b.copy_and_normalize()

    an.freeze()
    bn.freeze()

    assert an == bn
