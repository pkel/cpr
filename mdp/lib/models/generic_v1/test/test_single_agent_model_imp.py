from ..model import SingleAgentImp
from ..protocols import Bitcoin, Ghostdag
import random


def reward_and_progress(s: SingleAgentImp):
    history = s.defender.history()[1:]  # skip genesis
    rew = {None: 0, 0: 0, 1: 0}
    prg = 0
    for b in history:
        for m, amount in s.defender.coinbase(b):
            rew[m] += amount
        prg += s.defender.progress(b)
    rew.pop(None)
    return (rew, prg)


def sim_around_honest(n, *args, exp, alpha, gamma, **kwargs):
    s = SingleAgentImp(*args, **kwargs)

    for _ in range(n):
        if random.random() < exp:
            options = list(s.actions())
            action = options[random.randrange(len(options))]
        else:
            action = s.honest()

        s.apply(action, alpha=alpha, gamma=gamma)

    return reward_and_progress(s)


def per_protocol(*args, **kwargs):
    alpha_gamma = dict(alpha=0.33, gamma=0.5)

    rew, prg = sim_around_honest(1000, *args, **kwargs, **alpha_gamma, exp=0.0)
    rpp = rew[0] / prg
    assert 0.3 <= rpp <= 0.36

    rew, prg = sim_around_honest(250, *args, **kwargs, **alpha_gamma, exp=0.1)
    rpp = rew[0] / prg
    assert 0.25 <= rpp <= 0.4

    rew, prg = sim_around_honest(100, *args, **kwargs, **alpha_gamma, exp=0.5)


def test_bitcoin():
    random.seed(42)
    per_protocol(Bitcoin)


def test_ghostdag_3():
    random.seed(42)
    per_protocol(Ghostdag, k=3)
