from mdp import sum_to_one
from model import Model, PTO_wrapper, Transition
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
        assert horizon > 0

        self.model = PTO_wrapper(model, horizon=horizon)
        self.horizon = horizon

        self.set_exploration(eps=eps, eps_honest=eps_honest)

        self.state_id = None  # current integer state
        self.state_map = dict()  # maps model state to integer state
        self.state_value = []  # maps integer state to state-value estimate
        self.start_states = set()  # explored start states (integer)
        self.state_count = []  # visit counter for integer states; statistics
        self.n_states_visited = 0  # statistics
        self._state_actions = (
            []
        )  # state id -> (action id -> transition list); memoize model
        self._state_honest_action = []  # state id -> action id; memoize model
        self.unexplored_states = (
            dict()
        )  # states with an integer id but no entry in _state_actions

        # init state & state_id
        self.start_new_episode()

        # statistics
        self.episode = 0  # episode counter
        self.mean_progress = horizon

    def start_new_episode(self):
        self.ep_progress = 0  # statistics

        # Do Sutton and Barto's "Exploring Starts" in 50% of the cases.
        # TODO "Exploring Starts" needs evaluation
        if random.random() < 0.5 and len(self.state_map) > 0:
            self.state_id = random.randrange(len(self.state_map))
            return

        # TODO Cache this as well?
        state = sample(self.model.start(), lambda x: x[1])[0]
        self.state_id = self.map_state(state)
        self.start_states |= {self.state_id}

    def reset(self):
        self.episode += 1
        self.mean_progress -= self.mean_progress / self.episode  # statistics
        self.mean_progress += self.ep_progress / self.episode  # statistics
        self.start_new_episode()

    def set_exploration(self, *args, eps=None, eps_honest=None):
        if eps is None:
            eps = self.eps

        if eps_honest is None:
            eps_honest = self.eps_honest

        assert 0 <= eps <= 1
        assert 0 <= eps_honest <= 1

        self.eps = eps
        self.eps_honest = eps_honest

    def map_state(self, state):
        if state in self.state_map:
            return self.state_map[state]
        else:
            state_id = len(self.state_map)
            self.state_map[state] = state_id
            self.state_value.append(0)
            self.state_count.append(0)
            self._state_actions.append(None)
            self._state_honest_action.append(None)
            self.unexplored_states[state_id] = state
            return state_id

    def state_actions(self, state_id):
        if self._state_actions[state_id] is not None:
            return self._state_actions[state_id]
        else:
            state = self.unexplored_states[state_id]
            del self.unexplored_states[state_id]
            actions = []
            m_actions = self.model.actions(state)
            for a in m_actions:
                transitions = []
                for t in self.model.apply(a, state):
                    to_state_id = self.map_state(t.state)
                    integer_t = Transition(
                        state=to_state_id,
                        probability=t.probability,
                        reward=t.reward,
                        progress=t.progress,
                        effect=t.effect,
                    )
                    transitions.append(integer_t)

                assert sum_to_one([t.probability for t in transitions])
                actions.append(transitions)

            if len(actions) > 0:  # non-terminal state
                h = self.model.honest(state)
                self._state_honest_action[state_id] = m_actions.index(h)

            # cache and return
            self._state_actions[state_id] = actions
            return actions

    def start_value(self):
        v = 0
        for s, p in self.model.start():
            sid = self.map_state(s)
            v += p * self.state_value[sid]
        return v

    def step(self):
        state_id = self.state_id

        # statistics
        if self.state_count[state_id] == 0:
            # we visit this state for the first time
            self.n_states_visited += 1
        self.state_count[state_id] += 1

        # get possible actions
        actions = self.state_actions(state_id)
        n = len(actions)

        if n < 1:
            # no action available, terminal state
            self.reset()
            assert self.state_value[state_id] == 0
            return

        # unfold all available actions, tracking ...
        max_i = 0  # index of best action
        max_q = 0  # value of best action
        # ... and caching
        action_transitions = []  # transition lists for all actions

        for i, transitions in enumerate(actions):
            q = 0  # action value estimate
            for t in transitions:
                q += t.probability * (t.reward + self.state_value[t.state])

            action_transitions.append(transitions)

            if q > max_q:
                max_i = i
                max_q = q

        # update state-value estimate
        self.state_value[state_id] = max_q

        # epsilon greedy policy
        i = max_i
        x = random.random()
        if x < self.eps:
            # explore randomly
            i = random.randrange(n)
        elif x < self.eps + self.eps_honest:
            # explore along honest policy
            i = self._state_honest_action[state_id]

        # apply action & transition
        to = sample(action_transitions[i], lambda x: x.probability)
        self.state_id = to.state
        self.ep_progress += to.progress  # statistics
