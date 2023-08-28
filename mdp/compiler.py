from mdp import MDP, Transition
from model import Model
import math
import queue


def is_one(x):
    return math.isclose(x, 1, rel_tol=1e-12)


def check_transitions(lst):
    assert isinstance(lst, list)
    acc_prob = 0
    for t in lst:
        acc_prob += t.probability
    assert is_one(acc_prob)
    return True


class Compiler:
    def __init__(self, model: Model):
        self.model = model
        self.queue = queue.Queue()
        self.state_map = dict()  # maps state to integer
        self.action_map = dict()  # maps action to integer
        self.explored = set()  # ids of already explored states
        self.start_probabilities = dict()
        self._mdp = MDP()

        # insert start states
        for state, probability in model.start():
            assert state not in self.state_map
            # obtain id
            state_id = len(self.state_map)
            self.state_map[state] = state_id
            # record probability
            self.start_probabilities[state_id] = probability
            # schedule exploration
            self.queue.put(state)

        # check start probabilities
        assert is_one(sum(self.start_probabilities.values()))

    def explore(self, steps=1000) -> bool:
        for i in range(steps):
            if self.queue.empty():
                return False
            else:
                self.step()
        return True

    def step(self):
        state = self.queue.get()

        # do not explore twice
        if state in self.explored:
            return
        self.explored.add(state)

        # recall state id
        state_id = self.state_map[state]

        # explore possible actions
        for action in self.model.actions(state):
            # create or reuse action id
            if action in self.action_map:
                action_id = self.action_map[action]
            else:
                action_id = len(self.action_map)
                self.action_map[action] = action_id

            # apply action, iterate transitions
            transitions = self.model.apply(action, state)
            assert check_transitions(transitions)
            for to in transitions:
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
            self.queue.put(to.state)

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

        self._mdp.check()

        return self._mdp
