# Implementation of Selfish Mining model(s) as described by Sapirshtein et al.
# @ FC '16.

# https://doi.org/10.1007/978-3-662-54970-4_30

from dataclasses import dataclass, replace
from ..implicit_mdp import Action, Model, Transition
from .. import explicit_mdp as mdp

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
    def __init__(
        self,
        *args,
        alpha: float,
        gamma: float,
        maximum_fork_length: int,
        maximum_dag_size: int = 0,
    ):
        if alpha < 0 or alpha >= 0.5:
            raise ValueError("alpha must be between 0 and 0.5")
        if gamma < 0 or gamma > 1:
            raise ValueError("gamma must be between 0 and 1")

        self.alpha = alpha
        self.gamma = gamma
        self.mfl = maximum_fork_length
        self.mds = maximum_dag_size

    def __repr__(self):
        return (
            f"fc16sapirshtein.BitcoinSM("
            f"alpha={self.alpha}, "
            f"gamma={self.gamma}, "
            f"maximum_fork_length={self.mfl}, "
            f"maximum_dag_size={self.mds})"
        )

    def start(self) -> list[tuple[BState, float]]:
        s = []
        s.append((BState(a=1, h=0, fork=IRRELEVANT), self.alpha))
        s.append((BState(a=0, h=1, fork=IRRELEVANT), 1 - self.alpha))
        return s

    def truncate_state_space(self, s: BState) -> bool:
        # based on fork length; original approach
        if self.mfl > 0 and (s.a >= self.mfl or s.h >= self.mfl):
            return True

        # based on number of blocks (size of BlockDAG); implemented to allow
        # comparison to my generic models
        if self.mds > 0 and (s.a + s.h + 1 >= self.mds):
            return True

        return False

    def actions(self, s: BState) -> list[Action]:
        actions = []
        # truncation: allow mining only up to a certain point
        if not self.truncate_state_space(s):
            actions.append(WAIT)
        # override/match when it makes sense
        if s.a > s.h:
            actions.append(OVERRIDE)
        if s.a >= s.h and s.fork == RELEVANT:
            actions.append(MATCH)
        # giving up is always possible
        actions.append(ADOPT)
        return actions

    def apply_adopt(self, s: BState) -> list[Transition]:
        t = []
        snew = BState(a=1, h=0, fork=IRRELEVANT)
        t.append(Transition(state=snew, probability=self.alpha, reward=0, progress=s.h))
        snew = BState(a=0, h=1, fork=IRRELEVANT)
        t.append(
            Transition(state=snew, probability=1 - self.alpha, reward=0, progress=s.h)
        )
        return t

    def apply_override(self, s: BState) -> list[Transition]:
        assert s.a > s.h
        t = []
        snew = BState(a=s.a - s.h, h=0, fork=IRRELEVANT)
        t.append(
            Transition(
                state=snew,
                probability=self.alpha,
                reward=s.h + 1,
                progress=s.h + 1,
            )
        )
        snew = BState(a=s.a - s.h - 1, h=1, fork=RELEVANT)
        t.append(
            Transition(
                state=snew,
                probability=1 - self.alpha,
                reward=s.h + 1,
                progress=s.h + 1,
            )
        )
        return t

    def apply_wait(self, s: BState) -> list[Transition]:
        if s.fork == ACTIVE:
            return self._apply_active_wait(s)
        else:
            return self._apply_nonactive_wait(s)

    def _apply_nonactive_wait(self, s: BState) -> list[Transition]:
        assert s.fork != ACTIVE
        t = []
        snew = BState(a=s.a + 1, h=s.h, fork=IRRELEVANT)
        t.append(Transition(state=snew, probability=self.alpha, reward=0, progress=0.0))
        snew = BState(a=s.a, h=s.h + 1, fork=RELEVANT)
        t.append(
            Transition(
                state=snew,
                probability=1 - self.alpha,
                reward=0,
                progress=0,
            )
        )
        return t

    def _apply_active_wait(self, s: BState) -> list[Transition]:
        assert s.fork == ACTIVE
        return self._apply_active_wait_and_match(s)

    def apply_match(self, s: BState) -> list[Transition]:
        assert s.a >= s.h
        return self._apply_active_wait_and_match(s)

    def _apply_active_wait_and_match(self, s: BState) -> list[Transition]:
        t = []
        snew = BState(a=s.a + 1, h=s.h, fork=ACTIVE)
        t.append(Transition(state=snew, probability=self.alpha, reward=0, progress=0.0))
        snew = BState(a=s.a - s.h, h=1, fork=RELEVANT)
        t.append(
            Transition(
                state=snew,
                probability=self.gamma * (1 - self.alpha),
                reward=s.h,
                progress=s.h,
            )
        )
        snew = BState(a=s.a, h=s.h + 1, fork=RELEVANT)
        t.append(
            Transition(
                state=snew,
                probability=(1 - self.gamma) * (1 - self.alpha),
                reward=0,
                progress=0,
            )
        )
        return t

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

    def honest(self, s: BState) -> list[Action]:
        if s.a > s.h:
            return OVERRIDE
        else:
            return ADOPT

    def shutdown(self, s: BState) -> list[Transition]:
        # Abort attack in favor of attacker; go back to start.
        ts = []
        for snew, p in self.start():
            if s.h > s.a:
                ts.append(Transition(state=snew, probability=p, reward=0, progress=s.h))
            elif s.a > s.h:
                ts.append(
                    Transition(state=snew, probability=p, reward=s.a, progress=s.a)
                )
            elif s.a == s.h:
                ts.append(
                    Transition(
                        state=snew, probability=p * self.gamma, reward=s.a, progress=s.a
                    )
                )
                ts.append(
                    Transition(
                        state=snew,
                        probability=p * (1 - self.gamma),
                        reward=0,
                        progress=s.h,
                    )
                )
            else:
                raise Exception("logic error")
        assert mdp.sum_to_one([t.probability for t in ts])
        return ts


mappable_params = dict(alpha=0.125, gamma=0.25)


def map_params(m: mdp.MDP, *args, alpha: float, gamma: float):
    assert alpha >= 0 and alpha <= 1
    assert gamma >= 0 and gamma <= 1

    a = mappable_params["alpha"]
    g = mappable_params["gamma"]
    mapping = dict()
    mapping[a] = alpha
    mapping[1 - a] = 1 - alpha
    mapping[(1 - a) * g] = (1 - alpha) * gamma
    mapping[(1 - a) * (1 - g)] = (1 - alpha) * (1 - gamma)

    assert len(set(mapping.keys())) == 4, "mappable_params are not mappable"

    # map probabilities
    tab = []
    for actions in m.tab:
        new_actions = list()
        for act, transitions in enumerate(actions):
            new_transitions = []
            for t in transitions:
                new_t = replace(t, probability=mapping[t.probability])
                new_transitions.append(new_t)
            new_actions.append(new_transitions)
        tab.append(new_actions)

    start = dict()
    for state, prob in m.start.items():
        start[state] = mapping[prob]

    new = replace(m, start=start, tab=tab)

    assert new.check()
    return new
