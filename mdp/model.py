from dataclasses import dataclass
from typing import TypeVar
import math
import sympy

State = TypeVar("State")
Action = TypeVar("Action")


@dataclass(frozen=True)
class Transition:
    state: State
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


class Model:
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

    def apply(self, a: Action, s: State) -> TransitionList:
        """
        Define state transitions. Action a is applied to state s.
        """
        raise NotImplementedError

    def apply_invalid(self, s: State) -> TransitionList:
        """
        Define what happens on invalid actions. That is, actions taken from
        s which are not listed in actions(s).
        """
        raise NotImplementedError
