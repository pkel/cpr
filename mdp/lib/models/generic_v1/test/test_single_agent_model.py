from ..model import SingleAgent
from ..protocols import Bitcoin, Ghostdag
import random


def sim_around_honest(n, *args, exp, alpha, gamma, **kwargs):
    m = SingleAgent(*args, **kwargs, alpha=alpha, gamma=gamma)

    s = m.start()[0][0]  # state
    prg = 0.0
    rew = 0.0

    for _ in range(n):
        if random.random() < exp:
            options = list(m.actions(s))
            action = options[random.randrange(len(options))]
        else:
            action = m.honest(s)

        transitions = m.apply(action, s)

        t = random.choices(transitions, weights=[t.probability for t in transitions])[0]

        assert sum(t.probability for t in transitions) >= 0.9999

        s = t.state
        prg += t.progress
        rew += t.reward

    return rew, prg


# ---


def per_protocol(*args, **kwargs):
    alpha_gamma = dict(alpha=0.33, gamma=0.5)

    rew, prg = sim_around_honest(100, *args, **kwargs, **alpha_gamma, exp=0)
    rpp = rew / prg
    assert 0.3 <= rpp <= 0.4

    rew, prg = sim_around_honest(50, *args, **kwargs, **alpha_gamma, exp=0.5)

    # --
    sim_around_honest(
        50, *args, **kwargs, **alpha_gamma, exp=0.3, merge_isomorphic=True
    )
    sim_around_honest(
        50, *args, **kwargs, **alpha_gamma, exp=0.3, truncate_common_chain=True
    )


def test_bitcoin():
    random.seed(42)
    per_protocol(Bitcoin)


def test_ghostdag_3():
    random.seed(42)
    per_protocol(Ghostdag, k=3)
