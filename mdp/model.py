from dataclasses import dataclass
from protocol import Block, View
from typing import TypeVar
import math
import sympy

State = TypeVar("State")


class StateEditor(View):
    def load(self, state: State) -> None:
        raise NotImplementedError

    def save(self) -> State:
        raise NotImplementedError

    def topo_sort(self, s: set[Block]) -> list[Block]:
        raise NotImplementedError


@dataclass(frozen=True, order=True)
class Trace:
    pass


@dataclass(frozen=True)
class Transition:
    state: State
    trace: Trace
    probability: float
    reward: float
    progress: float


class TransitionList:
    """
    Simple wrapper around list[Transition] that checks that the probabilities
    some up to one.
    """

    def __init__(self, lst: list[Transition]):
        symbolic = False
        for t in lst:
            if isinstance(t.probability, sympy.Basic):
                symbolic = True
                break
        assert symbolic or math.isclose(
            sum([t.probability for t in lst]), 1.0, rel_tol=1e-12
        ), "invalid transition list"
        self.lst = lst


@dataclass(frozen=True)
class Action:
    pass


class Model:
    def __init__(self, editor: StateEditor):
        """
        States are immutable and densely packed. The editor can unpack states
        for editing. We intentionally use a single editor: editor.load() will
        not allocate memory.
        """
        raise NotImplementedError

    def start(self) -> TransitionList:
        """
        Define start states and initial probabilities. Rewards will be ignored.
        """
        raise NotImplementedError

    def actions(self, s: State) -> set[Action]:
        """
        Define valid actions.
        """
        raise NotImplementedError

    def apply(self, a: Action, s: State, t: Trace) -> TransitionList:
        """
        Define state transitions. Action a is applied to state s. Trace t may
        be used to track how (= which actions and intermediate states) the
        source state s has been explored. This can be useful for early
        termination. Trace also guided the exploration: state with smaller
        trace are explored first.
        """
        raise NotImplementedError

    def apply_invalid(self, s: State, t: Trace) -> TransitionList:
        """
        Define what happens on invalid actions. That is, actions taken from
        s which are not listed in actions(s).
        """
        raise NotImplementedError
