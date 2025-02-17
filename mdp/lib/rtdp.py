from . import explicit_mdp as mdp
from .explicit_mdp import sum_to_one
from .implicit_mdp import Model, Transition
import random
import sys


def sample(lst, p: lambda x: x[0]):
    weights = []
    population = []
    for x in lst:
        weights.append(p(x))
        population.append(x)
    return random.choices(population, weights, k=1)[0]


class State:
    def __init__(self):
        self.value = 0  # estimate of future rewards
        self.progress = 0  # estimate of future progress
        self.count = 0
        self.es_last_seen = -1
        self.actions = None  # action idx -> state hash transition list
        self.honest = None  # honest action id


class RTDP:
    def __init__(
        self,
        model: Model,
        *args,
        eps: float,
        eps_honest: float = 0,
        es: float = 0,
        es_threshold=500_000,
        state_hash_fn=None,  # collision resistant hash appropriate for model
    ):
        self.model = model

        self.set_exploration(eps=eps, eps_honest=eps_honest, es=es)

        # If states are significantly big (e.g. 1k) it makes sense to derive
        # fingerprints, use them as index for the state value estimate,
        # throwing away the full states asap. This optimization can be turned
        # on by providing a collision resistant hash function to the agent.
        # In this implementation I assume it's turned on.
        if state_hash_fn is None:
            self.hash_state = lambda x: x
        else:
            self.hash_state = state_hash_fn

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
        self.es_buf = dict()  # state hash -> full state
        self.es_threshold = es_threshold

        self.i = 0

        # start states
        self.start_states = list()  # list[tuple[float, hash, full_state]]
        for full_state, prob in self.model.start():
            state, state_hash = self.state_and_hash_of_full_state(full_state)
            self.start_states.append((prob, state_hash, full_state, state))

        # init state & state_id
        self.start_new_episode()

        # statistics
        self.n_episodes = 0
        self.progress_gamma999 = 0
        self.exploration_gamma9999 = 1
        self.state_size_gamma9999 = 0
        self.n_states_visited = 0

    def start_new_episode(self):
        # statistics
        self.episode_progress = 0  # statistics

        # Barto and Sutton's "exploring starts"
        if self.es > 0:
            if random.random() < self.es:
                candidates = []
                for state_hash, state in self.states.items():
                    if state.es_last_seen < 1:
                        continue
                    if self.i - state.es_last_seen < self.es_threshold:
                        candidates.append(self.es_buf[state_hash])
                    else:
                        # We won't need these anymore
                        self.es_buf.pop(state_hash, None)
                if len(candidates) > 0:
                    self.set_full_state(random.choice(candidates))
                    return

        # start from an actual start state otherwise
        self.set_full_state(sample(self.start_states, lambda x: x[0])[2])

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
        for prob, _hash, _full, state in self.start_states:
            v += prob * state.value
            p += prob * state.progress
        return v, p

    def set_full_state(self, full_state):
        self.full_state = full_state
        self.state, self.state_hash = self.state_and_hash_of_full_state(full_state)

    def step(self):
        self.i += 1
        full_state = self.full_state
        state = self.state

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
            #  assert state.value == 0
            #  assert state.progress == 0
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
        self.set_full_state(to.state)

        # ## Exploring starts
        if greedy:
            self.state.es_last_seen = self.i + 1
            self.es_buf[self.state_hash] = self.full_state

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
        state_hash = self.hash_state(full_state)

        if state_hash in self.states:
            state = self.states[state_hash]
        else:
            state = State()
            self.states[state_hash] = state
            state.value, state.progress = self.initial_value_estimate(full_state)

        return state, state_hash

    def initial_value_estimate(self, full_state):
        # We have a couple of options here.
        # - conservatively return 0
        # - return <large number> to generally encourage exploration (not monotonic!)
        # - guide exploration by evaluating the honest policy
        # - do a fair shutdown to get a partial estimate of the states potential

        v = 0.0
        p = 0.0
        for t in self.model.shutdown(full_state):
            immediate_v = t.reward
            immediate_p = t.progress

            state_hash = self.hash_state(t.state)
            if state_hash in self.states:
                state = self.states[state_hash]
                future_v = state.value
                future_p = state.progress
            else:
                future_v = 0
                future_p = 0

            v += t.probability * (immediate_v + future_v)
            p += t.probability * (immediate_p + future_p)

        return v, p

    def mdp(self, *args):
        # The agent operates on a partially explored MDP,
        # this function extracts this MDP and the best known policy.
        # During exploration, states are represented by hashes; in the returned
        # MDP states are represented by a range of integers starting from 0.

        state_id = dict()
        for s_id, s_hash in enumerate(self.states.keys()):
            state_id[s_hash] = s_id

        # iterate states; build mdp, policy, value estimate
        # terminal states will have policy None
        m = mdp.MDP()  # empty mdp
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
        for prob, state_hash, full_state, state in self.start_states:
            m.start[state_id[state_hash]] = prob

        assert m.check()
        assert m.n_states == len(self.states) + 1

        return dict(mdp=m, policy=policy, value=value)

    def _prep_state_id_and_terminal_state(self, state_id, terminal_state):
        # used in self.policy and self.value

        # derive integer ids or reuse the provided ones
        if state_id is None:
            state_id = dict()
            for s_id, s_hash in enumerate(self.state.keys()):
                state_id[s_hash] = s_id

        n_states = len(state_id)
        for s_id in state_id.values():
            assert s_id < n_states, "non-continuous state_id"

        if terminal_state is None:
            terminal_state = object()

        if terminal_state not in state_id:
            state_id[terminal_state] = n_states
            n_states += 1

        return state_id, terminal_state

    def policy(self, *args, state_id=None, terminal_state=None):
        # state_id argument: dict[state, int]; the policy will work on these state ids

        state_id, terminal_state = self._prep_state_id_and_terminal_state(
            state_id, terminal_state
        )
        n_states = len(state_id)

        # derive policy
        policy = [-1] * (n_states)  # -1 for terminal state
        for src_hash, src_state in self.states.items():
            src_id = state_id[src_hash]
            best_a = -1  # no action available / terminal state
            best_q = 0.0
            if src_state.actions is not None:
                # explored state:
                for a, transitions in enumerate(src_state.actions):
                    q = 0.0
                    for hash_t in transitions:
                        # policy
                        to_state = self.states[hash_t.state]
                        q += hash_t.probability * (hash_t.reward + to_state.value)

                    if q > best_q or best_a < 0:
                        best_q = q
                        best_a = a

                policy[src_id] = best_a
            else:
                # unexplored state:
                policy[src_id] = 0  # TODO what if this state is terminal?!

        return policy

    def value(self, *args, state_id=None, terminal_state=None):
        # state_id argument: dict[state, int]; the value estimate will be for these state ids

        state_id, terminal_state = self._prep_state_id_and_terminal_state(
            state_id, terminal_state
        )
        n_states = len(state_id)

        # extract value estimate
        value = [0.0] * (n_states)
        for src_hash, src_state in self.states.items():
            value[state_id[src_hash]] = src_state.value

        return value
