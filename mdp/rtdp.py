from collections import deque
import mdp
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
        self.value = 0  # estimate of future rewards
        self.progress = 0  # estimate of future progress
        self.count = 0
        self.actions = None  # action idx -> state hash transition list
        self.honest = None  # honest action id


class RTDP:
    def __init__(
        self,
        model: Model,
        *args,
        horizon: int,
        eps: float,
        eps_honest: float = 0,
        es: float = 0
    ):
        assert horizon > 0

        model = PTO_wrapper(model, horizon=horizon, terminal_state=b"")
        self.model = model
        self.horizon = horizon

        self.set_exploration(eps=eps, eps_honest=eps_honest, es=es)

        self.full_state = None  # current state, full model state
        self.states = dict()  # state hash -> State

        # Sutton and Barto propose 'exploring states', that is, starting
        # exploration / episodes from a random state. This ensures that all
        # states are visited regularly, independent of the policy; this ensures
        # their algorithm converges on the optimal policy.
        # Now, we explicitly do not want to visit all states. Nevertheless we
        # have the problem that an epsilon-soft policy implies that exploration
        # is focused on early states.
        # We overcome this by maintaining a set of good full states which are
        # worth using as starting states. What is a good state? We just use the
        # set of recently visited states.
        self.exploring_starts = deque(maxlen=100 * horizon)  # full states

        # start states
        self.start_states = list()  # list[tuple[float, hash, full_state]]
        for full_state, prob in self.model.start():
            state, state_hash = self.state_and_hash_of_full_state(full_state)
            self.start_states.append((prob, state_hash, full_state))

        # init state & state_id
        self.start_new_episode()

        # statistics
        self.n_episodes = 0
        self.progress_gamma999 = horizon
        self.exploration_gamma9999 = 1
        self.state_size_gamma9999 = 0
        self.n_states_visited = 0

    def start_new_episode(self):
        # statistics
        self.episode_progress = 0  # statistics

        # Barto and Sutton's "exploring starts"
        if self.es > 0 and len(self.exploring_starts) > 0:
            if random.random() < self.es:
                self.full_state = random.choice(self.exploring_starts)
                return

        # start from an actual start state otherwise
        self.full_state = sample(self.start_states, lambda x: x[0])[2]

    def reset(self):
        self.n_episodes += 1
        self.progress_gamma999 = (
            self.progress_gamma999 * 0.999 + 0.001 * self.episode_progress
        )
        self.start_new_episode()

    def set_exploration(self, *args, eps=None, eps_honest=None, es=None):
        if eps is not None:
            assert 0 <= eps <= 1
            self.eps = eps

        if eps_honest is not None:
            assert 0 <= eps_honest <= 1
            self.eps_honest = eps_honest

        if es is not None:
            assert 0 <= es <= 1
            self.es = es

    def start_value_and_progress(self):
        v = 0
        p = 0
        for full_state, prob in self.model.start():
            state, state_hash = self.state_and_hash_of_full_state(full_state)
            v += prob * state.value
            p += prob * state.progress
        return v, p

    def step(self):
        full_state = self.full_state
        state, state_hash = self.state_and_hash_of_full_state(full_state)

        # ## Statistics

        if state.count < 1:
            self.n_states_visited += 1
            self.exploration_gamma9999 *= 0.9999
            self.exploration_gamma9999 += 0.0001
        else:
            self.exploration_gamma9999 *= 0.9999

        self.state_size_gamma9999 *= 0.9999
        self.state_size_gamma9999 += 0.0001 * sys.getsizeof(full_state)

        state.count += 1  # increment visit counter for statistics

        # ## End of statistics

        # get possible actions
        actions = self.actions(state, full_state)
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
        max_p = 0  # progress of best action
        for i, transitions in enumerate(actions):
            q = 0  # action value estimate
            p = 0
            for t in transitions:
                to_state = self.states[t.state]
                q += t.probability * (t.reward + to_state.value)
                p += t.probability * (t.progress + to_state.progress)

            if q > max_q:
                max_i = i
                max_q = q
                max_p = p

        # update state-value and progress estimate
        state.value = max_q
        state.progress = max_p

        # state transition
        # eps-soft behaviour
        x = random.random()
        greedy = False
        if x < self.eps:
            # random exploration
            i = random.randrange(n_actions)
        elif x < self.eps + self.eps_honest:
            # honest exploration
            i = self.honest(state, full_state)
        else:
            # greedy step
            greedy = True
            i = max_i

        # NOTE there is some redundancy here: model.actions() and model.apply()
        # might have just been called from self.actions() if not cached. So
        # it's not obvious that caching actually helps us!
        a = self.model.actions(full_state)[i]
        to = sample(self.model.apply(a, full_state), lambda x: x.probability)
        self.episode_progress += to.progress  # statistics
        self.full_state = to.state

        # exploring starts
        if greedy:
            self.exploring_starts.append(to.state)

    def actions(self, state, full_state):
        if state.actions is not None:
            return state.actions

        # TODO since the switch to using state hashes to avoid storing the full
        # states I have not evaluated whether this caching is worth it. For
        # fast models it might well be that the caching adds more overhead than
        # it safes execution time.

        actions = []
        m_actions = self.model.actions(full_state)
        for a in m_actions:
            transitions = []
            for t in self.model.apply(a, full_state):
                to_state, to_state_hash = self.state_and_hash_of_full_state(t.state)
                transitions.append(
                    Transition(
                        state=to_state_hash,
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
            h = m_actions.index(self.model.honest(full_state))
            state.honest = h

        state.actions = actions
        return actions

    def honest(self, state, full_state):
        if state.honest is not None:
            return state.honest

        if state.actions is None:
            _ = self.actions(state, full_state)

        assert len(state.actions) > 0, "no honest action for terminal state"
        return state.honest

    def state_and_hash_of_full_state(self, full_state):
        state_hash = collision_resistant_hash(full_state)

        if state_hash in self.states:
            state = self.states[state_hash]
        else:
            state = State()
            self.states[state_hash] = state
            state.value = self.initial_value_estimate(full_state)

        return state, state_hash

    def initial_value_estimate(self, full_state):
        # We have a couple of options here.
        # - conservatively return 0
        # - return <large number> to generally encourage exploration (not monotonic!)
        # - guide exploration by evaluating the honest policy
        # - do a fair shutdown to get a partial estimate of the states potential

        value = 0
        for t in self.model.shutdown(full_state):
            value += t.probability * t.reward

        return value

    def mdp(self):
        # The agent operates on a partially explored MDP,
        # this function extracts this MDP and the best known policy.
        # During exploration, states are represented by hashes; in the returned
        # MDP states are represented by a range of integers starting from 0.

        # derive integer ids
        state_id = dict()
        for s_id, s_hash in enumerate(self.states.keys()):
            state_id[s_hash] = s_id

        # iterate states; build mdp, policy, value estimate
        # terminal states will have policy None
        m = mdp.MDP()
        n_states = len(self.states)
        policy = [-1] * (n_states + 1)  # -1 for terminal state
        terminal_state = n_states
        value = [0.0] * (n_states + 1)
        for src_hash, src_state in self.states.items():
            src_id = state_id[src_hash]
            value[src_id] = src_state.value
            best_a = -1  # no action available / terminal state
            best_q = 0.0
            if src_state.actions is not None:
                # explored state:
                for a, transitions in enumerate(src_state.actions):
                    # policy
                    q = 0.0
                    for hash_t in transitions:
                        # policy
                        to_state = self.states[hash_t.state]
                        q += hash_t.probability * (hash_t.reward + to_state.value)

                        # mdp
                        int_t = mdp.Transition(
                            destination=state_id[hash_t.state],
                            probability=hash_t.probability,
                            reward=hash_t.reward,
                            progress=hash_t.progress,
                            effect=hash_t.effect,
                        )
                        m.add_transition(src_id, a, int_t)

                    # policy
                    if q > best_q or best_a < 0:
                        best_q = q
                        best_a = a

                policy[src_id] = best_a
            else:
                # unexplored state:
                # assume single action, deterministic transition to terminal
                # state, handing out the initial state value estimate
                # TODO I think this is off. First, does it even matter what
                # value we set? An agent wants to avoid navigating into a
                # terminal state, at least for high horizons. Second, wouldn't
                # it be better to shutdown & restart & terminate based on
                # progress?

                # mdp
                int_t = mdp.Transition(
                    destination=terminal_state,
                    probability=1,
                    reward=src_state.value,
                    progress=0.0,
                    effect=None,
                )
                m.add_transition(src_id, 0, int_t)

                # policy
                policy[src_id] = 0

        # mdp: set start states
        assert len(m.start) == 0
        for prob, state_hash, full_state in self.start_states:
            m.start[state_id[state_hash]] = prob

        assert m.check()
        assert m.n_states == len(self.states) + 1

        return dict(mdp=m, policy=policy, value=value)
