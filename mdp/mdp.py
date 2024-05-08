from dataclasses import dataclass, field
from model import Effect
from time import time
from typing import Optional
import math
import numpy
import scipy


@dataclass(frozen=True, order=True)
class Transition:
    probability: float  # how likely is the transition?
    destination: int  # where do we transition to?
    reward: float  # MDP reward
    progress: float  # PTO progress
    effect: Optional[Effect] = None  # additional information


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

    def value_iteration(
        self, *args, max_iter=0, discount=1, eps=0, stop_delta=None, verbose=False
    ):
        assert discount <= 1 and discount > 0
        assert eps is not None or stop_delta is not None
        assert eps is None or eps >= 0
        assert stop_delta is None or stop_delta >= 0

        # abort condition of eps-optimal policy
        # https://github.com/aimacode/aima-java/blob/7ecee83 [...]/ValueIteration.java
        if stop_delta is None:
            stop_delta = eps * (1 - discount) / discount

        assert max_iter > 0 or stop_delta > 0 or verbose, "infinite iteration"

        start = time()

        value = numpy.zeros((2, self.n_states), dtype=float)
        progress = numpy.zeros((2, self.n_states), dtype=float)
        policy = numpy.zeros((2, self.n_states), dtype=int)

        i = 1
        while True:
            prev = i % 2
            next = (prev + 1) % 2

            for src, actions in enumerate(self.tab):
                best_v = 0.0
                best_p = 0.0
                best_a = -1  # no action possible
                for act, lst in actions.items():
                    if act < 0:
                        continue
                    this_v = 0.0
                    this_p = 0.0
                    for t in lst:
                        this_v += t.probability * (
                            t.reward + discount * value[prev, t.destination]
                        )
                        this_p += t.probability * (
                            t.progress + discount * progress[prev, t.destination]
                        )
                    if this_v >= best_v:  # intentionally, to not stick with action -1
                        best_v = this_v
                        best_p = this_p
                        best_a = act
                value[next, src] = best_v
                progress[next, src] = best_p
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

            if max_iter > 0 and i >= max_iter:
                break
            elif value_delta <= stop_delta:
                break
            else:
                i += 1

        if verbose:
            print()  # new line to finish verbose progress bar

        return dict(
            vi_discount=discount,
            vi_delta=value_delta,
            vi_stop_delta=stop_delta,
            vi_policy=policy[next,],
            vi_value=value[next,],
            vi_progress=progress[next,],
            vi_iter=i,
            vi_max_iter=max_iter,
            vi_time=time() - start,
        )

    def reachable_states(self, policy, *args, start_state=None):
        # find subset of states used by policy
        reachable = set()
        todo = set()

        if start_state is None:
            for s, prob in self.start.items():
                if prob > 0:
                    todo.add(s)
        else:
            todo.add(start_state)

        while len(todo) > 0:
            s = todo.pop()
            assert s not in reachable
            reachable.add(s)

            act = policy[s]
            if act < 0:
                # no action possible; terminal state
                continue
            for t in self.tab[s][act]:
                if t.probability == 0.0:
                    continue
                if t.destination in reachable:
                    continue
                else:
                    todo.add(t.destination)

        return reachable

    def markov_chain(self, policy, *args, start_state):
        reachable = self.reachable_states(policy, start_state=start_state)

        # map markov chain state <-> markov decision process state
        # mdp_state[mc state] = <mdp state>
        mdp_state = sorted(list(reachable))
        # mc_state[mdp state] = <mc state>
        mc_state = {mdp: mc for mc, mdp in enumerate(mdp_state)}

        # build matrices for transition probability, reward, and progress
        n = len(reachable)
        row = []
        col = []
        prb = []
        rew = []
        prg = []
        for mdp_s, mc_s in mc_state.items():
            act = policy[mdp_s]
            if act >= 0:
                for t in self.tab[mdp_s][act]:
                    if t.probability == 0.0:
                        continue

                    row.append(mc_s)
                    col.append(mc_state[t.destination])
                    prb.append(t.probability)
                    rew.append(t.reward)
                    prg.append(t.progress)
            else:
                # no action possible; terminal state
                row.append(mc_s)
                col.append(mc_s)
                prb.append(1.0)
                rew.append(0)
                prg.append(0)
        return dict(
            prb=scipy.sparse.coo_matrix((prb, (row, col)), shape=(n, n)),
            rew=scipy.sparse.coo_matrix((rew, (row, col)), shape=(n, n)),
            prg=scipy.sparse.coo_matrix((prg, (row, col)), shape=(n, n)),
            mdp_states=mdp_state,
        )

    def _steady_state_mc(self, prb):
        start = time()

        n = prb.shape[0]
        val = list(prb.data)
        row = list(prb.row)
        col = list(prb.col)

        # tutorial: https://math.stackexchange.com/a/2452452

        for s in range(n):
            # -1 on the diagonal
            row.append(s)
            col.append(s)
            val.append(-1)

            # all-ones column
            row.append(s)
            col.append(n)
            val.append(1)

        Q = scipy.sparse.csr_matrix((val, (row, col)), shape=(n, n + 1))
        QTQ = Q.dot(Q.transpose())
        bQT = numpy.ones(n)

        v = scipy.sparse.linalg.spsolve(QTQ, bQT)

        res = dict()

        if numpy.isnan(v[0]):
            # matrix singular, cannot solve system exactly
            # steady state is ambiguous or does not exists
            lsqr = scipy.sparse.linalg.lsqr(QTQ, bQT)
            itop = lsqr[1]
            if itop == 1:
                # v is an approximate solution; steady state is ambiguous
                pass
            else:
                # v is least squares approximation; steady state does not exist
                # But: finite markov chains always have a steady state!
                assert False, "something is off"

            v = lsqr[0]
            assert math.isclose(sum(v), 1, rel_tol=1e-5)
            v = v / sum(v)

            res["ss_lsqr_iter"] = lsqr[2]

        assert len(v) == n
        assert math.isclose(sum(v), 1), sum(v)

        res["ss"] = v
        res["ss_n"] = n
        res["ss_nonzero"] = len(v.nonzero()[0])
        res["ss_time"] = time() - start

        return res

    def steady_state(self, policy, *args, start_state):
        start = time()

        mc = self.markov_chain(policy, start_state=start_state)
        mc_ss = self._steady_state_mc(mc["prb"])

        # map steady state from mc state space back to mdp state space
        mdp_ss = numpy.zeros(self.n_states, dtype=float)
        for mc_state, mdp_state in enumerate(mc["mdp_states"]):
            mdp_ss[mdp_state] = mc_ss["ss"][mc_state]

        return dict(
            ss=mdp_ss,
            ss_reachable=len(mc_ss["ss"]),
            ss_nonzero=mc_ss["ss_nonzero"],
            ss_time=time() - start,
        )

    def policy_evaluation(
        self,
        policy,
        *args,
        theta,
        discount=1,
        around_state=None,
        max_iter=None,
    ):
        rew = numpy.zeros((2, self.n_states), dtype=float)
        prg = numpy.zeros((2, self.n_states), dtype=float)

        if around_state is None:
            included_states = self.reachable_states(policy, start_state=around_state)
        else:
            included_states = range(self.n_states)

        i = 1
        while True:
            prev = i % 2
            next = (prev + 1) % 2

            for src in included_states:
                a = policy[src]
                if a < 0:
                    continue
                r = 0.0
                p = 0.0
                for t in self.tab[src][a]:
                    r += t.probability * (
                        t.reward + discount * rew[prev, t.destination]
                    )
                    p += t.probability * (
                        t.progress + discount * prg[prev, t.destination]
                    )
                rew[next, src] = r
                prg[next, src] = p

            delta = numpy.abs(rew[next,] - rew[prev,]).max()

            if delta < theta:
                break

            if max_iter is not None and i >= max_iter:
                break

            i += 1

        return dict(pe_reward=rew[next,], pe_progress=prg[next,], pe_iter=i)
