from mdp import MDP, Transition
import numpy
import pynauty
import queue
import scipy
import sm
import xxhash

# Model and protocol spec use generic types which we instantiate here:
# - Block: int
# - State: bytes

N_PROP = 6


class StateEditor(sm.StateEditor):
    def __init__(self, max_blocks=32):
        shape = (max_blocks + N_PROP, max_blocks)
        self.buf = numpy.full(shape, False)
        self.n_blocks = 0
        # these are writeable views into the buffer
        self._preferred_by_attacker = self.buf[0,]
        self._preferred_by_defender = self.buf[1,]
        self._considered_by_attacker = self.buf[2,]
        self._known_to_defender = self.buf[3,]
        self._mined_by_defender = self.buf[4,]
        self._released_by_attacker = self.buf[5,]
        self._adj = self.buf[N_PROP:,]

    def clear(self):
        self.n_blocks = 0

    def _save(self):
        sub = self.buf[: self.n_blocks + N_PROP, : self.n_blocks]
        packed = numpy.packbits(sub, axis=None)
        with_size = numpy.append(packed, [numpy.uint8(self.n_blocks)])
        return with_size.tobytes()

    def save(self):
        return self._save()

        # _save plus safeguards
        binary = self._save()
        ic = self.isomorphism_class()
        if self.n_blocks > 0:
            subset = self.ancestors(self.preferred_by_attacker)
            subset.add(self.preferred_by_attacker)
            ics = self.isomorphism_class(subset)
        else:
            subset = None
            ics = None
        self.load(binary)
        assert self.isomorphism_class() == ic
        assert subset is None or self.isomorphism_class(subset) == ics
        return binary

    def load(self, s):
        with_size = numpy.frombuffer(s, dtype=numpy.uint8)
        self.n_blocks = with_size[-1]
        packed = with_size[:-1]
        size = (self.n_blocks + N_PROP) * self.n_blocks
        shape = (self.n_blocks + N_PROP, self.n_blocks)
        # TODO this triggers a redundant allocation for sub. Avoid?!
        # BUT: if we want it to be really fast we could do bit operations,
        # directly on a binary buffer. Or use a bitvector implementation. And
        # use a close-to-memory language like C or Rust. But it might not be
        # worth the effort!
        sub = numpy.unpackbits(packed, axis=None)[:size].reshape(shape).view(bool)
        self.buf[: (self.n_blocks + N_PROP), : self.n_blocks] = sub

    def __repr__(self):
        return repr(self.buf[: (self.n_blocks + N_PROP), : self.n_blocks])

    @property
    def preferred_by_attacker(self):
        assert self.n_blocks > 0, "operation not defined for empty state"
        for i in range(self.n_blocks):
            if self._preferred_by_attacker[i]:
                return i
        print(self)
        assert False, "invalid editor state"

    @property
    def preferred_by_defender(self):
        assert self.n_blocks > 0, "operation not defined for empty state"
        for i in range(self.n_blocks):
            if self._preferred_by_defender[i]:
                return i
        assert False, "invalid editor state"

    @property
    def considered_by_attacker(self):
        r = set()
        for i in range(self.n_blocks):
            if self._considered_by_attacker[i]:
                r.add(i)
        return r

    @property
    def known_to_defender(self):
        r = set()
        for i in range(self.n_blocks):
            if self._known_to_defender[i]:
                r.add(i)
        return r

    @property
    def mined_by_defender(self):
        r = set()
        for i in range(self.n_blocks):
            if self._mined_by_defender[i]:
                r.add(i)
        return r

    @property
    def released_by_attacker(self):
        r = set()
        for i in range(self.n_blocks):
            if self._released_by_attacker[i]:
                r.add(i)
        return r

    def set_considered_by_attacker(self, b):
        self._considered_by_attacker[b] = True

    def set_known_to_defender(self, b):
        assert b in self.mined_by_defender or b in self.released_by_attacker
        self._known_to_defender[b] = True

    def set_released_by_attacker(self, b):
        assert b not in self.mined_by_defender
        self._released_by_attacker[b] = True

    def set_preferred_by_attacker(self, b):
        assert b in self.considered_by_attacker
        for i in range(self.n_blocks):
            self._preferred_by_attacker[i] = b == i

    def set_preferred_by_defender(self, b):
        assert b in self.known_to_defender
        for i in range(self.n_blocks):
            self._preferred_by_defender[i] = b == i

    def invert(self, s):
        return set(range(self.n_blocks)) - s

    def append(self, parents, *args, mined_by_defender: bool):
        new = self.n_blocks
        self.n_blocks += 1
        # initialize parents and children with False
        for i in range(self.n_blocks):
            self._adj[new, i] = False
            self._adj[i, new] = False
        # set parents
        for p in parents:
            self._adj[new, p] = True
        # initialize properties
        self._mined_by_defender[new] = mined_by_defender
        self._released_by_attacker[new] = False
        self._considered_by_attacker[new] = False
        self._known_to_defender[new] = False
        self._preferred_by_attacker[new] = False
        self._preferred_by_defender[new] = False

        return new

    def parents(self, b):
        r = set()
        for i in range(self.n_blocks):
            if self._adj[b, i]:
                r.add(i)
        return r

    def children(self, b):
        r = set()
        for i in range(self.n_blocks):
            if self._adj[i, b]:
                r.add(i)
        return r

    def miner(self, b):
        if self._mined_by_defender[b]:
            return sm.DEFENDER
        else:
            return sm.ATTACKER

    def topo_sort(self, blocks):
        # blocks can only be appended, hence id's are already ordered
        return sorted(list(blocks))

    def canonical_order(self, subset=None):
        # nauty is a C library for finding canonical representations of graphs
        # and checking for isomorphism. pynauty provides Python bindings.
        #
        # This function derives a pynauty graph isomorphic to the edited state
        # or a subset thereof.

        # set default subset = all blocks
        if subset is None:
            subset = set(range(self.n_blocks))

        size = len(subset)

        # my code only works for nonempty graphs
        if size < 1:
            return dict()

        # pynauty expects integer keys starting from 0
        block_id = {block: id for id, block in enumerate(subset)}

        # Nauty works on colored graphs. To make it work we have to encode all
        # state into colors. How many do we need?
        #
        # There are six boolean properties.
        #   preferred_by_attacker: int
        #   preferred_by_defender: int
        #   ignored_by_attacker: set[int]
        #   withheld_by_attacker: set[int]
        #   mined_by_attacker: set[int]
        #   known_to_defender: set[int]
        # Naively, we'd need 2^6=64 colors. Since some combinations are not
        # possible, we can shrink that number:
        #   - preferred_by_defender implies known_to_defender
        #   - preferred_by_attacker implies considered_by_attacker
        #   - withheld_by_attacker implies !mined_by_defender
        #   - released_by_attacker implies !mined_by_defender
        # So, we can get away with three properties
        #   - defender_view: unknown, known, preferred
        #   - attacker_view: ignored, considered, preferred
        #   - withholding: mined_by_defender, withheld, released
        # and hence 3^3 = 27 colors.

        dv = [0] * size  # defender_view = unknown
        for x in self.known_to_defender & subset:
            dv[block_id[x]] = 1  # defender_view = known
        if self.preferred_by_defender in subset:
            dv[block_id[self.preferred_by_defender]] = 2  # defender_view = preferred

        av = [0] * size  # attacker_view = ignored
        for x in self.considered_by_attacker & subset:
            av[block_id[x]] = 1  # attacker_view = considered
        if self.preferred_by_attacker in subset:
            av[block_id[self.preferred_by_attacker]] = 2  # attacker_view = preferred

        w = [0] * size  # withholding = withheld
        for x in self.mined_by_defender & subset:
            w[block_id[x]] = 1  # withholding = mined_by_defender
        for x in self.released_by_attacker & subset:
            w[block_id[x]] = 2  # withholding = released

        # pynauty color representation: list of sets
        colors = [set() for _ in range(27)]  # vertex coloring
        for i in range(size):
            colors[(dv[i] * 9) + (av[i] * 3) + w[i]].add(i)

        # pynauty expects non-empty color sets
        vc = list()
        palette = list()  # required to reconstruct state from colored graph
        for i in range(27):
            if len(colors[i]) > 0:
                palette.append(i)
                vc.append(colors[i])

        # pynauty graph representation: adjacency dict
        ad = dict()
        for src in subset:
            ad[block_id[src]] = list()
            for dst in self.parents(src) & subset:
                ad[block_id[src]].append(block_id[dst])

        # init pynauty Graph
        g = pynauty.Graph(
            size,
            directed=True,
            adjacency_dict=ad,
            vertex_coloring=vc,
        )

        # find canonical labels and return
        relabel = pynauty.canon_label(g)
        return {b: relabel[block_id[b]] for b in subset}

    def isomorphism_class(self, subset=None):
        canonical_ids = self.canonical_order(subset)
        size = len(canonical_ids)
        arr = numpy.full((size + N_PROP, size), False)
        if size > 0:
            # copy data in canonical order from self.buf into arr
            idx = numpy.zeros(size, dtype=int)
            for block, canon in canonical_ids.items():
                idx[canon] = block
            for i in range(N_PROP):
                arr[i,] = self.buf[i, idx]
            arr[N_PROP:,] = self._adj[idx, idx]
        # convert to bytes and hash
        # WARNING: in principle, arr.tobytes() is a valid state. It can be
        # loaded into the editor. However, this would break a core assumption
        # that blocks are topologically sorted. See topo_sort() method.
        return xxhash.xxh3_128_digest(arr.tobytes())


class Compiler:
    def __init__(self, model: sm.Model):
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
        assert self.queue.qsize() == 0, "exploration in progress"
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
