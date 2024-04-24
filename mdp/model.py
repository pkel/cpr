from dataclasses import dataclass
from typing import Optional, TypeVar

State = TypeVar("State")
Action = TypeVar("Action")


# @dataclass(frozen=True, kw_only=True)  # requires Python 3.10
@dataclass(frozen=True)
class Effect:
    blocks_mined: float  # how many blocks have been mined? (0 or 1)
    common_atk_reward: float  # attacker reward on common chain
    common_def_reward: float  # defender reward on common chain
    common_progress: float  # progress made on common chain
    defender_rewrite_length: float  # number of history entries rewritten
    defender_rewrite_progress: float  # progress rewritten instead of length
    defender_progress: float  # progress made on defender's chain


@dataclass(frozen=True)
class Transition:
    probability: float  # how likely is the transition?
    state: State  # where do we transition to?
    reward: float  # effect.common_atk_reward
    progress: float  # effect.common_progress
    effect: Optional[Effect] = None


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

    def shutdown(self, s: State) -> list[Transition]:
        """
        Define a fair shutdown mechanism. We call this at the end of each
        episode.

        Example. For selfish mining we force the agent to release all blocks,
        then do one last round of communication, before calculating the final
        rewards. This encourages risk-taking in the light of probabilistic
        termination.
        """
        raise NotImplementedError

    def acc_effect(self, a: Effect, b: Effect) -> Effect:
        """
        When merging two steps, what's the accumulated effect?
        """
        raise NotImplementedError

    def honest(self, s: State) -> Action:
        """
        What would an honest participant do?
        """
        raise NotImplementedError


class PTO_wrapper(Model):
    def __init__(self, model, *args, horizon: int, terminal_state):
        assert horizon > 0
        assert isinstance(model, Model)
        assert not isinstance(model, PTO_wrapper)

        self.unwrapped = model
        self.terminal = terminal_state
        self.horizon = horizon

    def start(self):
        return self.unwrapped.start()

    def actions(self, state):
        if state is self.terminal:
            return []
        else:
            return self.unwrapped.actions(state)

    def apply(self, action, state):
        assert state is not self.terminal

        # update original transition list to include termination
        transitions = []
        for t in self.unwrapped.apply(action, state):
            if t.progress == 0.0:
                transitions.append(t)
            else:
                continue_p = (1.0 - (1.0 / self.horizon)) ** t.progress
                assert 0 < continue_p < 1
                # one transition for continuing
                continue_t = Transition(
                    probability=t.probability * continue_p,
                    state=t.state,
                    reward=t.reward,
                    progress=t.progress,
                    effect=t.effect,
                )
                transitions.append(continue_t)

                # multiple transitions for shutdown
                term_p = 1 - continue_p
                for st in self.unwrapped.shutdown(t.state):
                    term_e = self.unwrapped.acc_effect(t.effect, st.effect)
                    term_t = Transition(
                        probability=t.probability * term_p * st.probability,
                        state=self.terminal,
                        reward=t.reward + st.reward,
                        progress=t.progress + st.progress,
                        effect=term_e,
                    )
                    transitions.append(term_t)

        return transitions

    def honest(self, state):
        assert state is not self.terminal
        return self.unwrapped.honest(state)
