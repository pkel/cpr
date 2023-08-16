from dataclasses import dataclass
from protocol import Block, View
from typing import TypeVar

State = TypeVar("State")


class StateEditor(View):
    @property
    def attacker_prefers(self) -> Block:
        raise NotImplementedError

    @property
    def defender_prefers(self) -> Block:
        raise NotImplementedError

    @property
    def considered_by_attacker(self) -> set[Block]:
        raise NotImplementedError

    @property
    def known_to_defender(self) -> set[Block]:
        raise NotImplementedError

    @property
    def mined_by_attacker(self) -> set[Block]:
        raise NotImplementedError

    @property
    def withheld_by_attacker(self) -> set[Block]:
        raise NotImplementedError

    def set_considered_by_attacker(self, Block) -> None:
        raise NotImplementedError

    def set_known_to_defender(self, Block) -> None:
        raise NotImplementedError

    def set_preferred_by_attacker(self, Block) -> None:
        raise NotImplementedError

    def set_preferred_by_defender(self, Block) -> None:
        raise NotImplementedError

    def set_released_by_attacker(self, Block) -> None:
        raise NotImplementedError

    def invert(self, s: set[Block]) -> set[Block]:
        raise NotImplementedError

    def append(self, parents: set[Block], mined_by_attacker: bool) -> Block:
        raise NotImplementedError

    def clear(self) -> None:
        raise NotImplementedError

    def load(self, state: State) -> None:
        raise NotImplementedError

    def save(self) -> State:
        raise NotImplementedError


@dataclass(frozen=True)
class Trace:
    pass


@dataclass(frozen=True)
class Transition:
    state: State
    trace: Trace
    probability: float
    reward: float = 0.0


class TransitionList:
    """
    Simple wrapper around list[Transition] that checks that the probabilities
    some up to one.
    """

    def __init__(self, lst: list[Transition]):
        assert sum([t.probability for t in lst]) == 1.0, "invalid transition list"
        self.lst = lst


@dataclass(frozen=True)
class Action:
    pass


@dataclass(frozen=True)
class Priority:
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
        termination.
        """
        raise NotImplementedError

    def apply_invalid(self, s: State, t: Trace) -> TransitionList:
        """
        Define what happens on invalid actions. That is, actions taken from
        s but not listed in actions(s).
        """
        raise NotImplementedError

    def priority(self) -> Priority:
        """
        Guide exploration. States with low priority will be expanded first.
        """
        return Priority()
