# Implementation of Selfish Mining model(s) as described by Bar-Zur et al. at
# AFT '20.

# Roi Bar-Zur, Ittay Eyal, and Aviv Tamar. 2020. Efficient MDP Analysis for
# Selfish-Mining in Blockchains. In 2nd ACM Conference on Advances in Financial
# Technologies (AFT ’20), October 21–23, 2020, New York, NY, USA. ACM, New
# York, NY, USA, 19 pages. https://doi.org/10.1145/3419614.3423264

from dataclasses import dataclass
from model import Action, Model, Transition, TransitionList
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


@dataclass(order=True)
class Trace:
    pass


class Bitcoin(Model):
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

        # we don't use the trace, just return this one everywhere
        self.trace = Trace()

    def start(self) -> TransitionList:
        s = BState(a=0, h=0, fork=IRRELEVANT)
        t = Transition(state=s, trace=self.trace, reward=0, probability=1, progress=0)
        return TransitionList([t])

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
            actions.append(MATCH)
        # giving up is always possible
        actions.append(ADOPT)
        return actions

    def apply_wait(self, s: BState) -> TransitionList:
        t = []
        if s.fork != ACTIVE:
            # attacker mines block
            snew = BState(a=s.a + 1, h=s.h, fork=IRRELEVANT)
            t.append(
                Transition(
                    state=snew,
                    trace=self.trace,
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
                    trace=self.trace,
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
                    trace=self.trace,
                    probability=self.alpha,
                    reward=0.0,
                    progress=0,
                )
            )
            assert snew.a <= self.mfl

            # defender mines on top of attacker's chain
            # NOTE The paper assigns probability alpha * (1 - gamma) on p.8
            # right which must be a typo.
            snew = BState(a=s.a - s.h, h=1, fork=RELEVANT)
            t.append(
                Transition(
                    state=snew,
                    trace=self.trace,
                    probability=(1 - self.alpha) * self.gamma,
                    reward=s.h,
                    progress=s.h,
                )
            )

            # defender mines on top of public chain
            # NOTE The paper assigns probability alpha * (1 - gamma) on p.8
            # right which must be a typo.
            snew = BState(a=s.h, h=s.h + 1, fork=RELEVANT)
            t.append(
                Transition(
                    state=snew,
                    trace=self.trace,
                    probability=(1 - self.alpha) * (1 - self.gamma),
                    reward=0,
                    progress=0,
                )
            )
            assert snew.h <= self.mfl

        return TransitionList(t)

    def apply_adopt(self, s: BState) -> TransitionList:
        snew = BState(a=0, h=0, fork=s.fork)
        t = Transition(
            state=snew, trace=self.trace, probability=1.0, reward=0, progress=s.h
        )
        return TransitionList([t])

    def apply_override(self, s: BState) -> TransitionList:
        snew = BState(a=s.a - s.h - 1, h=0, fork=s.fork)
        t = Transition(
            state=snew,
            trace=self.trace,
            probability=1,
            reward=s.h + 1,
            progress=s.h + 1,
        )
        return TransitionList([t])

    def apply_match(self, s: BState) -> TransitionList:
        assert s.fork == RELEVANT
        assert s.a >= s.h
        snew = BState(a=s.a, h=s.h, fork=ACTIVE)
        t = Transition(
            state=snew, trace=self.trace, probability=1, reward=0, progress=0
        )
        return TransitionList([t])

    def apply(self, a: Action, s: BState, t: Trace) -> TransitionList:
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
                        )
                    )
                    new_transitions.append(
                        mdp.Transition(
                            destination=t.destination,
                            probability=(1 - term_prob) * t.probability,
                            reward=t.reward,
                            progress=t.progress,
                        )
                    )
                    n_transitions += 2

            tab[src][act] = new_transitions

    # construct check and return updated MDP
    new = mdp.MDP(
        n_states=n_states, n_transitions=n_transitions, tab=tab, n_actions=old.n_actions
    )
    new.check()
    return new
