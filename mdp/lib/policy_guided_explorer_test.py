from .policy_guided_explorer import Explorer
from .models import aft20barzur, fc16sapirshtein
from .models.generic_v0 import model as v0model, bitcoin as v0bitcoin
from .models.generic_v1 import model as v1model
from .models.generic_v1.protocols import bitcoin as v1bitcoin
from .models.generic_v1.protocols import ghostdag as v1ghostdag
from .implicit_mdp import PTO_wrapper

terminal_state = "terminal"


def test_aft20barzur():
    model = aft20barzur.BitcoinSM(alpha=0.25, gamma=0.33, maximum_fork_length=2**10)
    model = PTO_wrapper(model, horizon=100, terminal_state=terminal_state)

    explorer = Explorer(model, model.honest)

    n_states = []

    # distance 0
    m = explorer.mdp()
    n_states.append(m.n_states)

    # distance 1...
    for _ in range(5):
        explorer.explore_aside_policy()
        m = explorer.mdp()
        n_states.append(m.n_states)

    assert n_states == [4, 9, 16, 26, 38, 53]


def test_fc16sapirshtein():
    model = fc16sapirshtein.BitcoinSM(
        alpha=0.25, gamma=0.33, maximum_fork_length=2**10
    )
    model = PTO_wrapper(model, horizon=100, terminal_state=terminal_state)

    explorer = Explorer(model, model.honest)

    n_states = []

    # distance 0
    m = explorer.mdp()
    n_states.append(m.n_states)

    # distance 1...
    for _ in range(5):
        explorer.explore_aside_policy()
        m = explorer.mdp()
        n_states.append(m.n_states)

    assert n_states == [4, 8, 15, 24, 36, 50]


def test_v0_bitcoin():
    model = v0model.SelfishMining(
        v0bitcoin.Bitcoin(), alpha=0.25, gamma=0.33, maximum_size=2**10
    )
    model = PTO_wrapper(model, horizon=100, terminal_state=terminal_state)

    explorer = Explorer(model, model.honest)

    n_states = []

    # distance 0
    m = explorer.mdp()
    n_states.append(m.n_states)

    # distance 1...
    for _ in range(4):
        explorer.explore_aside_policy()
        m = explorer.mdp()
        n_states.append(m.n_states)

    assert n_states == [11, 37, 111, 279, 601]


def test_v1_bitcoin():
    model = v1model.SingleAgent(
        v1bitcoin.Protocol, alpha=0.25, gamma=0.33, loop_honest=True
    )
    model = PTO_wrapper(model, horizon=100, terminal_state=terminal_state)

    explorer = Explorer(model, model.honest)

    n_states = []

    # distance 0
    m = explorer.mdp()
    n_states.append(m.n_states)

    # distance 1...
    for _ in range(2):
        explorer.explore_aside_policy()
        m = explorer.mdp()
        n_states.append(m.n_states)

    assert n_states == [6, 27, 185]


def test_v1_ghostdag3():
    model = v1model.SingleAgent(
        v1ghostdag.Protocol, k=3, alpha=0.25, gamma=0.33, loop_honest=True
    )
    model = PTO_wrapper(model, horizon=100, terminal_state=terminal_state)

    explorer = Explorer(model, model.honest)

    n_states = []

    # distance 0
    m = explorer.mdp()
    n_states.append(m.n_states)

    # distance 1...
    for _ in range(2):
        explorer.explore_aside_policy()
        m = explorer.mdp()
        n_states.append(m.n_states)

    assert n_states == [6, 32, 264]


def test_v1_bitcoin_tcc():
    model = v1model.SingleAgent(
        v1bitcoin.Protocol,
        alpha=0.25,
        gamma=0.33,
        collect_garbage=True,
        truncate_common_chain=True,
    )
    model = PTO_wrapper(model, horizon=100, terminal_state=terminal_state)

    explorer = Explorer(model, model.honest)

    n_states = []

    # distance 0
    m = explorer.mdp()
    n_states.append(m.n_states)

    # distance 1...
    for _ in range(2):
        explorer.explore_aside_policy()
        m = explorer.mdp()
        n_states.append(m.n_states)

    assert n_states == [7, 14, 24]


def test_v1_ghostdag3_tcc():
    model = v1model.SingleAgent(
        v1ghostdag.Protocol,
        k=3,
        alpha=0.25,
        gamma=0.33,
        collect_garbage=True,
        truncate_common_chain=True,
    )
    model = PTO_wrapper(model, horizon=100, terminal_state=terminal_state)

    explorer = Explorer(model, model.honest)

    n_states = []

    # distance 0
    m = explorer.mdp()
    n_states.append(m.n_states)

    # distance 1...
    for _ in range(2):
        explorer.explore_aside_policy()
        m = explorer.mdp()
        n_states.append(m.n_states)

    assert n_states == [7, 41, 305]


def test_v1_bitcoin_mi():
    model = v1model.SingleAgent(
        v1bitcoin.Protocol,
        alpha=0.25,
        gamma=0.33,
        merge_isomorphic=True,
        collect_garbage=True,
        truncate_common_chain=True,
    )
    model = PTO_wrapper(model, horizon=100, terminal_state=terminal_state)

    explorer = Explorer(model, model.honest)

    n_states = []

    # distance 0
    m = explorer.mdp()
    n_states.append(m.n_states)

    # distance 1...
    for _ in range(2):
        explorer.explore_aside_policy()
        m = explorer.mdp()
        n_states.append(m.n_states)

    assert n_states == [7, 14, 24]


def test_v1_ghostdag3_mi():
    model = v1model.SingleAgent(
        v1ghostdag.Protocol,
        k=3,
        alpha=0.25,
        gamma=0.33,
        merge_isomorphic=True,
        collect_garbage=True,
        truncate_common_chain=True,
    )
    model = PTO_wrapper(model, horizon=100, terminal_state=terminal_state)

    explorer = Explorer(model, model.honest)

    n_states = []

    # distance 0
    m = explorer.mdp()
    n_states.append(m.n_states)

    # distance 1...
    for _ in range(2):
        explorer.explore_aside_policy()
        m = explorer.mdp()
        n_states.append(m.n_states)

    assert n_states == [7, 37, 230]
