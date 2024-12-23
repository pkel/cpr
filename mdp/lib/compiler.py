from .explicit_mdp import MDP, Transition, sum_to_one
from .implicit_mdp import Model
import queue


class Compiler:
    def __init__(self, model: Model):
        self.model = model
        self.queue = queue.Queue()
        self.state_map = dict()  # maps state to integer
        self.action_map = dict()  # maps action to integer
        self.explored = set()  # ids of already explored states
        self._mdp = MDP()

        # insert start states
        for state, probability in model.start():
            assert state not in self.state_map
            # obtain id
            state_id = len(self.state_map)
            self.state_map[state] = state_id
            # record probability
            self._mdp.start[state_id] = probability
            # schedule exploration
            self.queue.put(state)

    @property
    def n_states(self):
        return len(self.state_map)

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
        for action_id, action in enumerate(self.model.actions(state)):
            # apply action, iterate transitions
            transitions = self.model.apply(action, state)
            assert sum_to_one([t.probability for t in transitions])
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
            effect=to.effect,
        )
        self._mdp.add_transition(state_id, action_id, t)

    def mdp(self):
        # exploration might be incomplete
        while self.queue.qsize() > 0:
            self.step()

        self._mdp.check()

        return self._mdp
