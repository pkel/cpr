from dataclasses import dataclass, field
import math
import numpy


@dataclass(frozen=True, order=True)
class Transition:
    destination: int
    probability: float
    reward: float
    progress: float


action = int
state = int


def sum_to_one(x):
    return math.isclose(sum(x), 1, rel_tol=1e-15)


@dataclass()
class MDP:
    n_states: int = 0
    n_transitions: int = 0
    n_actions: int = 0
    tab: list[dict[action, list[Transition]]] = field(default_factory=list)
    start: dict[int, float] = field(default_factory=dict)

    def __repr__(self):
        s = self.n_states
        a = self.n_actions
        t = self.n_transitions
        return f"MDP of size {s} / {a} / {t} / {t/s:.1f}"

    def add_transition(self, src: state, act: action, t: Transition):
        dst = t.destination
        assert src >= 0
        assert dst >= 0
        # grow n_states and tab on demand
        max_id = max(src, dst)
        if max_id >= len(self.tab):
            for i in range(len(self.tab), max_id + 1):
                self.tab.append(dict())
                self.n_states += 1
            assert max_id == len(self.tab) - 1
        assert self.n_states == len(self.tab)
        # grow n_actions on demand
        self.n_actions = max(self.n_actions, act + 1)
        # create transition list on demand
        if act not in self.tab[src]:
            self.tab[src][act] = list()
        # append transition and count
        self.tab[src][act].append(t)
        self.n_transitions += 1

    def check(self, *args):
        # start states
        assert sum_to_one(self.start.values())
        for state in self.start.keys():
            assert state >= 0 and state < self.n_states, state
        # check that outgoing probabilities sum up to one
        for src in range(self.n_states):
            for act, transitions in self.tab[src].items():
                assert sum_to_one([t.probability for t in transitions]), f"{src}/{act}"
        # check continuity of actions / states & number of transitions
        act_seen = [False for _ in range(self.n_actions)]
        state_seen = [False for _ in range(self.n_states)]
        n_transitions = 0
        for src in range(self.n_states):
            state_seen[src] = True
            for act, transitions in self.tab[src].items():
                act_seen[act] = True
                for t in transitions:
                    n_transitions += 1
                    state_seen[t.destination] = True
        assert all(act_seen)
        assert all(state_seen)
        assert n_transitions == self.n_transitions
        # check reachability of states TODO
        return True

    def value_iteration(self, *args, n_iter=0, value_eps=0, discount=1, verbose=False):
        assert n_iter > 0 or value_eps > 0 or verbose, "infinite iteration"

        value = numpy.zeros((2, self.n_states), dtype=float)
        policy = numpy.zeros((2, self.n_states), dtype=int)

        i = 1
        while True:
            prev = i % 2
            next = (prev + 1) % 2

            for src, actions in enumerate(self.tab):
                best_v = 0.0
                best_a = -1  # no action possible
                for act, lst in actions.items():
                    if act < 0:
                        continue
                    this_v = 0.0
                    for t in lst:
                        this_v += t.probability * (
                            t.reward + discount * value[prev, t.destination]
                        )
                    if this_v >= best_v:  # intentionally, to not stick with action -1
                        best_v = this_v
                        best_a = act
                value[next, src] = best_v
                policy[next, src] = best_a
                assert best_a >= 0 or len(actions) == 0

            value_delta = numpy.abs(value[next,] - value[prev,]).max()
            policy_change = (policy[next,] != policy[prev,]).sum() / self.n_states * 100
            if verbose:
                print(
                    f"\riteration {i}: value delta {value_delta:g}, "
                    f"policy change {policy_change:.2f}%",
                    end="",
                )

            if n_iter > 0 and i >= n_iter:
                break
            elif value_delta <= value_eps:
                break
            else:
                i += 1

        if verbose:
            print()  # new line to finish verbose progress bar

        return dict(value=value[next,], policy=policy[next,], iter=i)
