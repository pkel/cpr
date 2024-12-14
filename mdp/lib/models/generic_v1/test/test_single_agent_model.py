from ..model import SingleAgent
from ..protocols import Bitcoin, Byzantium, Ethereum, Ghostdag, Parallel
from ....policy_guided_explorer import Explorer
import random
import pytest


def sim_around_honest(n, *args, exp, alpha, gamma, verbose=False, **kwargs):
    m = SingleAgent(*args, **kwargs, alpha=alpha, gamma=gamma)

    s = m.start()[0][0]  # state
    prg = 0.0
    rew = 0.0

    if verbose:
        print("start state")
        print(s)
        print(f"prg = {prg}, rew = {rew}")

    for _ in range(n):
        if random.random() < exp:
            options = list(m.actions(s))
            action = options[random.randrange(len(options))]
        else:
            action = m.honest(s)

        transitions = m.apply(action, s)

        assert sum(t.probability for t in transitions) >= 0.9999

        t = random.choices(transitions, weights=[t.probability for t in transitions])[0]

        s = t.state
        prg += t.progress
        rew += t.reward

        if verbose:
            print("-" * 42)
            print("state")
            print(s)
            print(f"prg = {prg}, rew = {rew}")

    return rew, prg, s


# ---


def simulate_protocol(*args, **kwargs):
    alpha_gamma = dict(alpha=0.33, gamma=0.5)

    rew, prg, s = sim_around_honest(100, *args, **kwargs, **alpha_gamma, exp=0)
    rpp = rew / prg
    assert 0.3 <= rpp <= 0.4

    rew, prg, s = sim_around_honest(50, *args, **kwargs, **alpha_gamma, exp=0.5)

    # --
    sim_around_honest(
        50, *args, **kwargs, **alpha_gamma, exp=0.3, merge_isomorphic=True
    )
    sim_around_honest(
        50, *args, **kwargs, **alpha_gamma, exp=0.3, truncate_common_chain=True
    )


def test_sim_bitcoin():
    random.seed(42)
    simulate_protocol(Bitcoin)


def test_sim_byzantion():
    random.seed(42)
    simulate_protocol(Byzantium)


def test_sim_ethereum():
    random.seed(42)
    simulate_protocol(Ethereum)


def test_sim_ghostdag_3():
    random.seed(42)
    simulate_protocol(Ghostdag, k=3)


@pytest.mark.skip
def test_sim_parallel_3():
    random.seed(42)
    # TODO the loop honest heuristic does not work for this protocol!
    simulate_protocol(Parallel, k=3, truncate_common_chain=True)


# ---


def finite_exploration(*args, **kwargs):
    m = SingleAgent(*args, **kwargs, alpha=0.3, gamma=0.2)
    e = Explorer(m, m.honest)

    cap = dict(max_states=100)

    # distance 0
    e.explore_along_policy(**cap)

    # distance 1
    e.explore_aside_policy(**cap)
    e.explore_along_policy(**cap)


def test_explore_bitcoin():
    finite_exploration(Bitcoin, loop_honest=True)


def test_explore_bitcoin_mi():
    finite_exploration(Bitcoin, merge_isomorphic=True, loop_honest=True)


def test_explore_bitcoin_tcc():
    finite_exploration(Bitcoin, collect_garbage=True, truncate_common_chain=True)


def test_explore_ghostdag_3():
    finite_exploration(Ghostdag, k=3, loop_honest=True)


def test_explore_ghostdag_3_mi():
    finite_exploration(Ghostdag, k=3, merge_isomorphic=True, loop_honest=True)


def test_explore_ghostdag_3_tcc():
    finite_exploration(Ghostdag, k=3, truncate_common_chain=True)
