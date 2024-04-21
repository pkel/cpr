# Implementation of Selfish Mining model(s) as described by Bar-Zur et al. at
# AFT '20.

# Roi Bar-Zur, Ittay Eyal, and Aviv Tamar. 2020. Efficient MDP Analysis for
# Selfish-Mining in Blockchains. In 2nd ACM Conference on Advances in Financial
# Technologies (AFT ’20), October 21–23, 2020, New York, NY, USA. ACM, New
# York, NY, USA, 19 pages. https://doi.org/10.1145/3419614.3423264

# Checked against author's implementation:
# https://github.com/roibarzur/pto-selfish-mining/blob/89c408638c9c875457d596dcd30fe82114160422/blockchain_mdps/bitcoin_model.py

from dataclasses import dataclass, replace
from model import Action, Model, Transition
import mdp

# Bitcoin action space
ADOPT = 0
OVERRIDE = 1
MATCH = 2
WAIT = 3

# Bitcoin match-action state
IRRELEVANT = 0
RELEVANT = 1
ACTIVE = 2


@dataclass(frozen=True, order=True)
class BState:  # Bitcoin State
    a: int  # length of the miner's secret chain
    h: int  # number of blocks in the public chain since last fork
    fork: int  # one of the above IRRELEVANT RELEVANT ACTIVE

    def __post_init__(self):
        assert self.a >= 0
        assert self.h >= 0


class BitcoinSM(Model):
    def __init__(self, *args, alpha: float, gamma: float, maximum_fork_length: int):
        if alpha < 0 or alpha >= 0.5:
            raise ValueError("alpha must be between 0 and 1")
        if gamma < 0 or gamma > 1:
            raise ValueError("gamma must be between 0 and 1")
        if maximum_fork_length <= 0:
            raise ValueError("maximum_fork_length must be greater 0")

        self.alpha = alpha
        self.gamma = gamma
        self.mfl = maximum_fork_length

    def __repr__(self):
        return (
            f"aft20barzur.BitcoinSM("
            f"alpha={self.alpha}, "
            f"gamma={self.gamma}, "
            f"maximum_fork_length={self.mfl})"
        )

    def start(self) -> list[tuple[BState, float]]:
        s = BState(a=0, h=0, fork=IRRELEVANT)
        return [(s, 1)]

    def actions(self, s: BState) -> list[Action]:
        actions = []
        # truncation: allow mining only up to a certain point
        if self.mfl < 1 or (s.a < self.mfl and s.h < self.mfl):
            actions.append(WAIT)
        # override/match when it makes sense
        if s.a > s.h:
            actions.append(OVERRIDE)
        if s.a >= s.h and s.fork == RELEVANT:
            # NOTE, the paper once says a >= h (p.8 right) and once says a == h
            # (p.8 left). I think MATCH can be a good choice even if a > h for
            # high gamma. Thus I do a >= h here.
            # In the author implementation they do a >= h as well.
            actions.append(MATCH)
        # giving up is always possible
        actions.append(ADOPT)
        return actions

    def honest(self, s: BState) -> list[Action]:
        if s.a > s.h:
            return OVERRIDE
        else:
            return ADOPT

    def apply_wait(self, s: BState) -> list[Transition]:
        t = []
        if s.fork != ACTIVE:
            # attacker mines block
            snew = BState(a=s.a + 1, h=s.h, fork=IRRELEVANT)
            t.append(
                Transition(
                    state=snew,
                    probability=self.alpha,
                    reward=0.0,
                    progress=0,
                )
            )
            assert snew.a <= self.mfl

            # defender mines block
            snew = BState(a=s.a, h=s.h + 1, fork=RELEVANT)
            t.append(
                Transition(
                    state=snew,
                    probability=1.0 - self.alpha,
                    reward=0.0,
                    progress=0,
                )
            )
            assert snew.h <= self.mfl
        else:
            # attacker mines block
            snew = BState(a=s.a + 1, h=s.h, fork=ACTIVE)
            t.append(
                Transition(
                    state=snew,
                    probability=self.alpha,
                    reward=0.0,
                    progress=0,
                )
            )
            assert snew.a <= self.mfl

            # defender mines on top of attacker's chain
            # NOTE The paper assigns probability alpha * gamma on p.8
            # right which must be a typo.
            # The author implementation does it like here.
            snew = BState(a=s.a - s.h, h=1, fork=RELEVANT)
            t.append(
                Transition(
                    state=snew,
                    probability=(1 - self.alpha) * self.gamma,
                    reward=s.h,
                    progress=s.h,
                )
            )

            # defender mines on top of public chain
            # NOTE The paper assigns probability alpha * (1 - gamma) on p.8
            # right which must be a typo.
            # The author implementation does it like here.
            snew = BState(a=s.a, h=s.h + 1, fork=RELEVANT)
            t.append(
                Transition(
                    state=snew,
                    probability=(1 - self.alpha) * (1 - self.gamma),
                    reward=0,
                    progress=0,
                )
            )
            assert snew.h <= self.mfl

        return t

    def apply_adopt(self, s: BState) -> list[Transition]:
        snew = BState(a=0, h=0, fork=IRRELEVANT)
        t = Transition(state=snew, probability=1.0, reward=0, progress=s.h)
        return [t]

    def apply_override(self, s: BState) -> list[Transition]:
        assert s.a > s.h
        snew = BState(a=s.a - s.h - 1, h=0, fork=IRRELEVANT)
        t = Transition(
            state=snew,
            probability=1,
            reward=s.h + 1,
            progress=s.h + 1,
        )
        return [t]

    def apply_match(self, s: BState) -> list[Transition]:
        assert s.fork == RELEVANT
        assert s.a >= s.h
        snew = BState(a=s.a, h=s.h, fork=ACTIVE)
        t = Transition(state=snew, probability=1, reward=0, progress=0)
        return [t]

    def apply(self, a: Action, s: BState) -> list[Transition]:
        # handle action
        if a == ADOPT:
            return self.apply_adopt(s)
        if a == OVERRIDE:
            return self.apply_override(s)
        if a == MATCH:
            return self.apply_match(s)
        if a == WAIT:
            return self.apply_wait(s)
        assert False, "invalid action"


def ptmdp(old: mdp.MDP, *args, horizon: int):
    """
    Transform given MDP into a probabilistically terminating MDP.

    We add one terminal state (at the end / with the highest id).
    """
    assert horizon > 0

    # terminal state is new last state
    terminal = old.n_states
    n_states = old.n_states + 1

    # setup new tab, all empty
    tab = [dict() for _ in range(n_states)]
    n_transitions = 0

    # iterate old transitions, split progress > 0 transition in two
    for src, actions in enumerate(old.tab):
        for act, transitions in actions.items():
            new_transitions = list()
            for t in transitions:
                if t.progress == 0.0:
                    new_transitions.append(t)
                    n_transitions += 1
                else:
                    Dt = t.progress
                    H = horizon
                    term_prob = 1.0 - ((1.0 - (1.0 / H)) ** Dt)
                    new_transitions.append(
                        mdp.Transition(
                            destination=terminal,
                            probability=term_prob * t.probability,
                            reward=t.reward,
                            progress=t.progress,
                            effect=t.effect,
                        )
                    )
                    new_transitions.append(
                        mdp.Transition(
                            destination=t.destination,
                            probability=(1 - term_prob) * t.probability,
                            reward=t.reward,
                            progress=t.progress,
                            effect=t.effect,
                        )
                    )
                    n_transitions += 2

            tab[src][act] = new_transitions

    # construct check and return updated MDP
    new = mdp.MDP(
        n_states=n_states,
        n_transitions=n_transitions,
        tab=tab,
        n_actions=old.n_actions,
        start=old.start,
    )
    new.check()
    return new


mappable_params = dict(alpha=0.125, gamma=0.25)


def map_params(m: mdp.MDP, *args, alpha: float, gamma: float):
    assert alpha >= 0 and alpha <= 1
    assert gamma >= 0 and gamma <= 1

    a = mappable_params["alpha"]
    g = mappable_params["gamma"]
    mapping = dict()
    mapping[1] = 1
    mapping[a] = alpha
    mapping[1 - a] = 1 - alpha
    mapping[(1 - a) * g] = (1 - alpha) * gamma
    mapping[(1 - a) * (1 - g)] = (1 - alpha) * (1 - gamma)

    assert len(set(mapping.keys())) == 5, "mappable_params are not mappable"

    # map probabilities
    tab = []
    for actions in m.tab:
        new_actions = dict()
        for act, transitions in actions.items():
            new_transitions = []
            for t in transitions:
                new_t = replace(t, probability=mapping[t.probability])
                new_transitions.append(new_t)
            new_actions[act] = new_transitions
        tab.append(new_actions)

    start = dict()
    for state, prob in m.start.items():
        start[state] = mapping[prob]

    new = replace(m, start=start, tab=tab)

    assert new.check()
    return new
