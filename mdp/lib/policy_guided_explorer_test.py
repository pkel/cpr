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

    # distance 0
    m = explorer.mdp()
    assert m.n_states == 4

    # distance 1
    explorer.explore_aside_policy()
    m = explorer.mdp()
    assert m.n_states == 9

    # distance 2
    explorer.explore_aside_policy()
    m = explorer.mdp()
    assert m.n_states == 16

    # distance 3
    explorer.explore_aside_policy()
    m = explorer.mdp()
    assert m.n_states == 26

    # distance 4
    explorer.explore_aside_policy()
    m = explorer.mdp()
    assert m.n_states == 38

    # distance 5
    explorer.explore_aside_policy()
    m = explorer.mdp()
    assert m.n_states == 53


def test_fc16sapirshtein():
    model = fc16sapirshtein.BitcoinSM(
        alpha=0.25, gamma=0.33, maximum_fork_length=2**10
    )
    model = PTO_wrapper(model, horizon=100, terminal_state=terminal_state)

    explorer = Explorer(model, model.honest)

    # distance 0
    m = explorer.mdp()
    assert m.n_states == 4

    # distance 1
    explorer.explore_aside_policy()
    m = explorer.mdp()
    assert m.n_states == 8

    # distance 2
    explorer.explore_aside_policy()
    m = explorer.mdp()
    assert m.n_states == 15

    # distance 3
    explorer.explore_aside_policy()
    m = explorer.mdp()
    assert m.n_states == 24

    # distance 4
    explorer.explore_aside_policy()
    m = explorer.mdp()
    assert m.n_states == 36

    # distance 5
    explorer.explore_aside_policy()
    m = explorer.mdp()
    assert m.n_states == 50


def test_v0_bitcoin():
    model = v0model.SelfishMining(
        v0bitcoin.Bitcoin(), alpha=0.25, gamma=0.33, maximum_size=2**10
    )
    model = PTO_wrapper(model, horizon=100, terminal_state=terminal_state)

    explorer = Explorer(model, model.honest)

    # distance 0
    m = explorer.mdp()
    assert m.n_states == 11

    # distance 1
    explorer.explore_aside_policy()
    m = explorer.mdp()
    assert m.n_states == 37

    # distance 2
    explorer.explore_aside_policy()
    m = explorer.mdp()
    assert m.n_states == 111

    # distance 3
    explorer.explore_aside_policy()
    m = explorer.mdp()
    assert m.n_states == 279

    # distance 4
    explorer.explore_aside_policy()
    m = explorer.mdp()
    assert m.n_states == 601


def test_v1_bitcoin():
    model = v1model.SingleAgent(
        v1bitcoin.Protocol, alpha=0.25, gamma=0.33, loop_honest=True
    )
    model = PTO_wrapper(model, horizon=100, terminal_state=terminal_state)

    explorer = Explorer(model, model.honest)

    # distance 0
    m = explorer.mdp()
    assert m.n_states == 5

    # distance 1
    explorer.explore_aside_policy()
    m = explorer.mdp()
    assert m.n_states == 24

    # distance 2
    explorer.explore_aside_policy()
    m = explorer.mdp()
    assert m.n_states == 169

    # distance 3
    # explorer.explore_aside_policy()
    # m = explorer.mdp()
    # assert m.n_states == 1081


def test_v1_ghostdag3():
    model = v1model.SingleAgent(
        v1ghostdag.Protocol, k=3, alpha=0.25, gamma=0.33, loop_honest=True
    )
    model = PTO_wrapper(model, horizon=100, terminal_state=terminal_state)

    explorer = Explorer(model, model.honest)

    # distance 0
    m = explorer.mdp()
    assert m.n_states == 5

    # distance 1
    explorer.explore_aside_policy()
    m = explorer.mdp()
    assert m.n_states == 23

    # distance 2
    explorer.explore_aside_policy()
    m = explorer.mdp()
    assert m.n_states == 197

    # distance 3
    # explorer.explore_aside_policy()
    # m = explorer.mdp()
    # assert m.n_states == 1585


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

    # distance 0
    m = explorer.mdp()
    assert m.n_states == 6

    # distance 1
    explorer.explore_aside_policy()
    m = explorer.mdp()
    assert m.n_states == 27

    # distance 2
    explorer.explore_aside_policy()
    m = explorer.mdp()
    assert m.n_states == 152


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

    # distance 0
    m = explorer.mdp()
    assert m.n_states == 6

    # distance 1
    explorer.explore_aside_policy()
    m = explorer.mdp()
    assert m.n_states == 31

    # distance 2
    explorer.explore_aside_policy()
    m = explorer.mdp()
    assert m.n_states == 219


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

    # distance 0
    m = explorer.mdp()
    assert m.n_states == 6

    # distance 1
    explorer.explore_aside_policy()
    m = explorer.mdp()
    assert m.n_states == 34

    # distance 2
    explorer.explore_aside_policy()
    m = explorer.mdp()
    assert m.n_states == 185


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

    # distance 0
    m = explorer.mdp()
    assert m.n_states == 6

    # distance 1
    explorer.explore_aside_policy()
    m = explorer.mdp()
    assert m.n_states == 28

    # distance 2
    explorer.explore_aside_policy()
    m = explorer.mdp()
    assert m.n_states == 164
