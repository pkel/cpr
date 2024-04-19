from mdp import sum_to_one
from model import Model
import random


def sample(lst, p: lambda x: x[0]):
    weights = []
    population = []
    for x in lst:
        weights.append(p(x))
        population.append(x)
    return random.choices(population, weights, k=1)[0]


class MCVI:
    def __init__(self, model: Model, *args, horizon: int, eps: float):
        assert 0 < eps < 1
        assert horizon > 0

        self.model = model
        self.horizon = horizon
        self.eps = eps

        self.state = None  # current model state
        self.state_id = None  # current integer state
        self.state_map = dict()  # maps model state to integer state
        self._state_value = []  # maps integer state to state-value estimate
        self.start_states = set()  # explored start states (integer)

        # init state & state_id
        self.start_new_episode()

        # statistics
        self.episode = 0  # episode counter
        self.mean_progress = horizon

    def start_new_episode(self):
        self.state = sample(self.model.start(), lambda x: x[1])[0]
        self.state_id = self.map_state(self.state)
        self.start_states |= {self.state_id}
        self.ep_progress = 0

    def reset(self):
        self.episode += 1
        self.mean_progress -= self.mean_progress / self.episode
        self.mean_progress += self.ep_progress / self.episode
        self.start_new_episode()

    def map_state(self, state):
        if state in self.state_map:
            return self.state_map[state]
        else:
            state_id = len(self.state_map)
            self.state_map[state] = state_id
            self._state_value.append(0)
            assert self._state_value[state_id] == 0
            return state_id

    def state_value(self, state):
        if state in self.state_map:
            state_id = self.state_map[state]
            return self._state_value[state_id]
        else:
            return 0

    def start_value(self):
        v = 0
        for s, p in self.model.start():
            v += p * self.state_value(s)
        return v

    def step(self):
        state = self.state
        state_id = self.state_id

        # get possible actions
        actions = self.model.actions(state)
        n = len(actions)
        assert n > 0
        # TODO handle terminal states w/o actions

        # unfold all actions, tracking ...
        max_i = 0  # index of best action
        max_q = 0  # value of best action
        # ... and caching
        action_transitions = []  # transition lists for all actions

        for i, action in enumerate(actions):
            transitions = self.model.apply(action, state)
            assert sum_to_one([t.probability for t in transitions])

            q = 0  # action value estimate
            for t in transitions:
                tp = self.termination_probability(t.progress)
                # When taking the action, the system terminates with
                # probability tp. When it terminates, the future rewards are
                # zero. Thus we discount q accordingly.
                q += (1 - tp) * t.probability * (t.reward + self.state_value(t.state))
                # NOTE. This looks like the usual discount factor used for
                # non-episodic / continuous problems! Is it equivalent?

            action_transitions.append(transitions)

            if q > max_q:
                max_i = i
                max_q = q

        # update state-value estimate
        self._state_value[state_id] = max_q

        # epsilon greedy policy
        i = max_i
        if random.random() < self.eps:
            # explore randomly
            i = random.randrange(n)

        # apply action & transition
        to = sample(action_transitions[i], lambda x: x.probability)
        if random.random() < self.termination_probability(to.progress):
            self.reset()
        else:
            self.state = to.state
            self.state_id = self.map_state(self.state)
            self.ep_progress += to.progress

    def termination_probability(self, progress: float):
        return 1.0 - (1.0 - 1.0 / self.horizon) ** progress
