from dataclasses import dataclass
from typing import TypeVar

State = TypeVar("State")
Action = TypeVar("Action")


@dataclass(frozen=True)
class Transition:
    state: State
    probability: float
    reward: float
    progress: float


class Model:
    def start(self) -> list[tuple[State, float]]:
        """
        Define start states and initial probabilities.
        """
        raise NotImplementedError

    def actions(self, s: State) -> set[Action]:
        """
        Define valid actions.
        """
        raise NotImplementedError

    def apply(self, a: Action, s: State) -> list[Transition]:
        """
        Define state transitions. Action a is applied to state s.
        """
        raise NotImplementedError
