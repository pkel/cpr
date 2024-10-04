from ..model import SingleAgentImp
from ..protocols import Bitcoin


def test_copy():
    a = SingleAgentImp(Bitcoin)
    b = a.copy()

    a.dag.append([0], 0)
    a.attacker.deliver(1)
    a.defender.deliver(1)

    # a changes as we expect:
    assert a.dag.all_blocks() == {0, 1}
    assert a.dag.parents(1) == [0]
    assert a.attacker.visible == {0, 1}
    assert a.defender.visible == {0, 1}
    assert a.attacker.children(0) == {1}
    assert a.defender.children(0) == {1}
    assert a.attacker.state.head == 1
    assert a.defender.state.head == 1

    # b remains unchanged:
    assert b.dag.all_blocks() == {0}
    assert len(b.dag._parents) == 1
    assert b.attacker.visible == {0}
    assert b.defender.visible == {0}
    assert b.attacker.children(0) == set()
    assert b.defender.children(0) == set()
    assert b.attacker.state.head == 0
    assert b.defender.state.head == 0

    b.dag.append([0, 0], 1)  # invalid BTC block; useful only for this test
    b.attacker.deliver(1)

    # a remains unchanged
    assert a.dag.all_blocks() == {0, 1}
    assert a.dag.parents(1) == [0]
    assert a.attacker.visible == {0, 1}
    assert a.defender.visible == {0, 1}
    assert a.attacker.children(0) == {1}
    assert a.defender.children(0) == {1}
    assert a.attacker.state.head == 1
    assert a.defender.state.head == 1

    # b changes as we expect:
    assert b.dag.all_blocks() == {0, 1}
    assert b.dag.parents(1) == [0, 0]
    assert b.attacker.visible == {0, 1}
    assert b.defender.visible == {0}
    assert b.attacker.children(0) == {1}
    assert b.defender.children(0) == set()
    assert b.attacker.state.head == 1
    assert b.defender.state.head == 0
