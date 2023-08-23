from mdp import MDP, Transition
from model import Model
import queue
import scipy


class Compiler:
    def __init__(self, model: Model):
        self.model = model
        self.queue = queue.PriorityQueue()
        self.state_map = dict()  # maps state to integer
        self.action_map = dict()  # maps action to integer
        self.explored = set()  # ids of already explored states
        self.start_probabilities = dict()
        self._mdp = MDP()

        # insert start states
        for start in model.start().lst:
            assert start.state not in self.state_map
            # obtain id
            state_id = len(self.state_map)
            self.state_map[start.state] = state_id
            # record probability
            self.start_probabilities[state_id] = start.probability
            # schedule exploration
            self.queue.put((start.trace, start.state))

    def explore(self, steps=1000) -> bool:
        for i in range(steps):
            if self.queue.empty():
                return False
            else:
                self.step()
        return True

    def step(self):
        trace, state = self.queue.get()

        # do not explore twice
        if state in self.explored:
            return
        self.explored.add(state)

        # recall state id
        state_id = self.state_map[state]

        # explore invalid action (id = -1)
        # for to in self.model.apply_invalid(state, trace).lst:
        #     self.handle_transition(state_id, -1, to)

        # explore possible actions
        for action in self.model.actions(state):
            # create or reuse action id
            if action in self.action_map:
                action_id = self.action_map[action]
            else:
                action_id = len(self.action_map)
                self.action_map[action] = action_id

            # apply action, iterate transitions
            for to in self.model.apply(action, state, trace).lst:
                self.handle_transition(state_id, action_id, to)

    def handle_transition(self, state_id, action_id, to):
        # identify target state
        if to.state in self.state_map:
            # target state has id already
            to_id = self.state_map[to.state]
        else:
            # we see target state for the first time
            # create state id
            to_id = len(self.state_map)
            self.state_map[to.state] = to_id
            # schedule recursive exploration
            self.queue.put((to.trace, to.state))

        # record transition
        t = Transition(
            destination=to_id,
            probability=to.probability,
            reward=to.reward,
            progress=to.progress,
        )
        self._mdp.add_transition(state_id, action_id, t)

    def record_transition(self, *args, src, act, dst, prb, rew, prg):
        t = Transition(destination=dst, probability=prb, reward=rew, progress=prg)
        self._mdp.add_transition(src, act, t)

    def mdp(self):
        # exploration might be incomplete
        while self.queue.qsize() > 0:
            self.step()

        # checks only work for non-symbolic MDPs
        if hasattr(self.model, "symbolic") and not self.model.symbolic:
            self._mdp.check()

        return self._mdp

    def mdp_matrices(self):
        assert False, "method not maintained"
        # create sparse matrix MDP suitable for pymdptoolbox
        assert self.queue.qsize() == 0, "exploration in progress"
        assert (
            not self.model.symbolic
        ), "cannot generate matrices for symbolic parameters"
        # init temp vectors
        S = len(self.state_map)
        A = len(self.action_map)
        row = [[] for _ in range(A)]
        col = [[] for _ in range(A)]
        p = [[] for _ in range(A)]
        r = [[] for _ in range(A)]
        # write transitions
        valid_actions = set()
        invalid_transitions = [[] for _ in range(S)]
        for a, src, dst, prob, rew in self.transitions:
            if a >= 0:
                valid_actions.add((src, a))
                row[a].append(src)
                col[a].append(dst)
                p[a].append(prob)
                r[a].append(rew)
            else:
                invalid_transitions[src].append((dst, prob, rew))
        # handle invalid actions
        for a in range(A):
            for src in range(S):
                if (src, a) in valid_actions:
                    continue
                for dst, prob, rew in invalid_transitions[src]:
                    row[a].append(src)
                    col[a].append(dst)
                    p[a].append(prob)
                    r[a].append(rew)
        # create sparse matrices
        matrix = scipy.sparse.csr_matrix
        for a in range(A):
            p[a] = matrix((p[a], (row[a], col[a])), shape=(S, S))
            r[a] = matrix((r[a], (row[a], col[a])), shape=(S, S))
        return (p, r)
