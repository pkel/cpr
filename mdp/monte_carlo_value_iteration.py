from mdp import sum_to_one
from model import Model, PTO_wrapper
import random


def sample(lst, p: lambda x: x[0]):
    weights = []
    population = []
    for x in lst:
        weights.append(p(x))
        population.append(x)
    return random.choices(population, weights, k=1)[0]


class MCVI:
    def __init__(
        self, model: Model, *args, horizon: int, eps: float, eps_honest: float = 0
    ):
        assert 0 < eps < 1
        assert 0 <= eps_honest < 1
        assert horizon > 0

        self.model = PTO_wrapper(model, horizon=horizon)
        self.horizon = horizon
        self.eps = eps
        self.eps_honest = eps_honest

        self.state = None  # current model state
        self.state_id = None  # current integer state
        self.state_map = dict()  # maps model state to integer state
        self._state_value = []  # maps integer state to state-value estimate
        self.start_states = set()  # explored start states (integer)
        self.state_count = []  # visit counter for integer states; statistics

        # init state & state_id
        self.start_new_episode()

        # statistics
        self.episode = 0  # episode counter
        self.mean_progress = horizon

    def start_new_episode(self):
        self.state = sample(self.model.start(), lambda x: x[1])[0]
        self.state_id = self.map_state(self.state)
        self.start_states |= {self.state_id}
        self.ep_progress = 0  # statistics

    def reset(self):
        self.episode += 1
        self.mean_progress -= self.mean_progress / self.episode  # statistics
        self.mean_progress += self.ep_progress / self.episode  # statistics
        self.start_new_episode()

    def map_state(self, state):
        if state in self.state_map:
            return self.state_map[state]
        else:
            state_id = len(self.state_map)
            self.state_map[state] = state_id
            self._state_value.append(0)
            self.state_count.append(0)
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

        self.state_count[state_id] += 1  # statistics

        # get possible actions
        actions = self.model.actions(state)
        n = len(actions)

        if n < 1:
            # no action available, terminal state
            self.reset()
            assert self._state_value[state_id] == 0
            return

        # unfold all available actions, tracking ...
        max_i = 0  # index of best action
        max_q = 0  # value of best action
        # ... and caching
        action_transitions = []  # transition lists for all actions

        for i, action in enumerate(actions):
            transitions = self.model.apply(action, state)
            assert sum_to_one([t.probability for t in transitions])

            q = 0  # action value estimate
            for t in transitions:
                q += t.probability * (t.reward + self.state_value(t.state))

            action_transitions.append(transitions)

            if q > max_q:
                max_i = i
                max_q = q

        # update state-value estimate
        self._state_value[state_id] = max_q

        # epsilon greedy policy
        i = max_i
        x = random.random()
        if x < self.eps:
            # explore randomly
            i = random.randrange(n)
        elif x < self.eps + self.eps_honest:
            # explore along honest policy
            a = self.model.honest(state)
            i = actions.index(a)

        # apply action & transition
        to = sample(action_transitions[i], lambda x: x.probability)
        self.state = to.state
        self.state_id = self.map_state(self.state)
        self.ep_progress += to.progress  # statistics
