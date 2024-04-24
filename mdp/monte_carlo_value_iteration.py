from mdp import sum_to_one
from model import Model, PTO_wrapper, Transition
import random
import xxhash
import sys


def sample(lst, p: lambda x: x[0]):
    weights = []
    population = []
    for x in lst:
        weights.append(p(x))
        population.append(x)
    return random.choices(population, weights, k=1)[0]


def collision_resistant_hash(x):
    try:
        return xxhash.xxh128(x).hexdigest()
    except TypeError:
        # x not bytes-like, play it safe
        return x


class State:
    def __init__(self):
        self.value = 0
        self.count = 0
        self._actions = None  # action idx -> state hash transition list
        self._honest = None  # honest action id

    def actions(self, full_state, model):
        if self._actions is not None:
            return self._actions

        # TODO since the switch to using state hashes to avoid storing the full
        # states I have not evaluated whether this caching is worth it. For
        # fast models it might well be that the caching adds more overhead than
        # it safes execution time.

        actions = []
        m_actions = model.actions(full_state)
        for a in m_actions:
            transitions = []
            for t in model.apply(a, full_state):
                transitions.append(
                    Transition(
                        state=collision_resistant_hash(t.state),
                        probability=t.probability,
                        reward=t.reward,
                        progress=t.progress,
                        effect=t.effect,
                    )
                )

            assert sum_to_one([t.probability for t in transitions])
            actions.append(transitions)

        if len(actions) > 0:
            # non-terminal state has honest action
            h = m_actions.index(model.honest(full_state))
            self._honest = h

        self._actions = actions
        return actions

    def honest(self, full_state, model):
        if self._honest is not None:
            return self._honest

        if self._actions is None:
            _ = self.actions(full_state, model)

        assert len(self._actions) > 0, "no honest action for terminal state"
        return self._honest


class MCVI:
    def __init__(
        self,
        model: Model,
        *args,
        horizon: int,
        eps: float,
        eps_honest: float = 0,
        eps_es: float = 0
    ):
        assert horizon > 0

        model = PTO_wrapper(model, horizon=horizon, terminal_state=b"")
        self.model = model
        self.horizon = horizon

        self.set_exploration(eps=eps, eps_honest=eps_honest, eps_es=eps_es)

        self.full_state = None  # current state, full model state
        self.states = dict()  # state hash -> State

        # We will record good full states such that we can start exploration
        # there. Sutton and Barton call this 'exploring starts'
        self.exploring_starts = dict()  # state hash -> full state

        # init state & state_id
        self.start_new_episode()

        # statistics
        self.n_episodes = 0
        self.progress_gamma999 = horizon
        self.exploration_gamma9999 = 1
        self.state_size_gamma9999 = 0

    def start_new_episode(self):
        # statistics
        self.episode_progress = 0  # statistics

        # Barton and Sutton's "exploring starts"
        if self.eps_es > 0 and len(self.exploring_starts) > 0:
            if random.random < self.eps_es:
                self.full_state = random.sample(list(self.exploring_starts.values()))
                return

        # start from an actual start state otherwise
        self.full_state = sample(self.model.start(), lambda x: x[1])[0]

    def reset(self):
        self.n_episodes += 1
        self.progress_gamma999 = (
            self.progress_gamma999 * 0.999 + 0.001 * self.episode_progress
        )
        self.start_new_episode()

    def set_exploration(self, *args, eps=None, eps_honest=None, eps_es=None):
        if eps is not None:
            assert 0 <= eps <= 1
            self.eps = eps

        if eps_honest is not None:
            assert 0 <= eps_honest <= 1
            self.eps_honest = eps_honest

        if eps_es is not None:
            assert 0 <= eps_es <= 1
            self.eps_es = eps_es

    def state_hash_value(self, state_hash):
        if state_hash in self.states:
            return self.states[state_hash].value
        else:
            return 0

    def start_value(self):
        v = 0
        for full_state, prob in self.model.start():
            state_hash = collision_resistant_hash(full_state)
            v += prob * self.state_hash_value(state_hash)
        return v

    def step(self):
        full_state = self.full_state
        state_hash = collision_resistant_hash(full_state)

        if state_hash in self.states:
            state = self.states[state_hash]
            self.exploration_gamma9999 *= 0.9999
        else:
            state = State()
            self.states[state_hash] = state
            self.exploration_gamma9999 *= 0.9999
            self.exploration_gamma9999 += 0.0001

        self.state_size_gamma9999 *= 0.9999
        self.state_size_gamma9999 += 0.0001 * sys.getsizeof(full_state)

        state.count += 1  # increment visit counter for statistics

        # get possible actions
        actions = state.actions(full_state, self.model)
        n_actions = len(actions)

        if n_actions < 1:
            # no action available, terminal state
            self.reset()
            assert state.value == 0
            return

        # value iteration step:
        # consider all available actions, tracking ...
        max_i = 0  # index of best action
        max_q = 0  # value of best action
        for i, transitions in enumerate(actions):
            q = 0  # action value estimate
            for t in transitions:
                q += t.probability * (t.reward + self.state_hash_value(t.state))

            if q > max_q:
                max_i = i
                max_q = q

        # update state-value estimate
        state.value = max_q

        # exploring starts heuristic:
        # we try to record such states that have better than honest value
        # TODO estimate the q for honest behaviour w/o using the model internals
        if max_q > self.model.unwrapped.alpha * self.horizon * 0.99:
            self.exploring_starts[state_hash] = full_state

        # state transition
        # eps-soft behaviour
        x = random.random()
        if x < self.eps:
            # random exploration
            i = random.randrange(n_actions)
        elif x < self.eps + self.eps_honest:
            # honest exploration
            i = state.honest(full_state, self.model)
        else:
            # greedy step
            i = max_i

        # NOTE there is some redundancy here: model.actions() and model.apply()
        # might have just been called from state.actions() if not cached. So
        # it's not obvious that caching actually helps us!
        a = self.model.actions(full_state)[i]
        to = sample(self.model.apply(a, full_state), lambda x: x.probability)
        self.episode_progress += to.progress  # statistics
        self.full_state = to.state
