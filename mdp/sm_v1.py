from dataclasses import dataclass
from model import Model, State, Transition, TransitionList
from protocol import Block, Protocol, View
import numpy
import pynauty
import sympy
import xxhash

ATTACKER = 0
DEFENDER = 1
assert ATTACKER != DEFENDER


@dataclass
class Config:
    alpha: float
    gamma: float
    protocol: Protocol

    invalid_reward: float = 0.0
    truncate_on_pow: int = -1  # TODO. rename to max_blocks or similar

    # expected protocol runtime before stochastic termination
    # (same scale as protocol.progress) set <= 0 to disable termination
    horizon: float = 0.0

    def __post_init__(self):
        self.symbolic = False
        for x in [self.alpha, self.gamma, self.horizon, self.invalid_reward]:
            if isinstance(x, sympy.Basic):
                self.symbolic = True
                print("WARNING: disabling safeguards due to symbolic parameters")
                return
        #  if not symbolic, then parameters should be set correctly
        if self.alpha < 0 or self.alpha > 1:
            raise ValueError("alpha must be between 0 and 1")
        if self.gamma < 0 or self.gamma > 1:
            raise ValueError("gamma must be between 0 and 1")


class StateEditor(View):
    @property
    def preferred_by_attacker(self) -> Block:
        raise NotImplementedError

    @property
    def preferred_by_defender(self) -> Block:
        raise NotImplementedError

    @property
    def considered_by_attacker(self) -> set[Block]:
        raise NotImplementedError

    @property
    def known_to_defender(self) -> set[Block]:
        raise NotImplementedError

    @property
    def mined_by_defender(self) -> set[Block]:
        raise NotImplementedError

    @property
    def released_by_attacker(self) -> set[Block]:
        raise NotImplementedError

    def set_considered_by_attacker(self, b: Block) -> None:
        raise NotImplementedError

    def set_known_to_defender(self, b: Block) -> None:
        raise NotImplementedError

    def set_preferred_by_attacker(self, b: Block) -> None:
        raise NotImplementedError

    def set_preferred_by_defender(self, b: Block) -> None:
        raise NotImplementedError

    def set_released_by_attacker(self, b: Block) -> None:
        raise NotImplementedError

    def invert(self, s: set[Block]) -> set[Block]:
        raise NotImplementedError

    def append(self, parents: set[Block], *args, mined_by_defender: bool) -> Block:
        """
        Create a new block. Blocks are initially ignored by the attacker and
        unknown to the defender. Attacker blocks are marked as withheld.
        """
        raise NotImplementedError

    def load(self, state: State) -> None:
        raise NotImplementedError

    def save(self) -> State:
        raise NotImplementedError

    def clear(self) -> None:
        """
        Remove all blocks from the state.
        """
        raise NotImplementedError

    def topo_sort(self, s: set[Block]) -> list[Block]:
        raise NotImplementedError


@dataclass(frozen=True)
class Action:
    pass


@dataclass(frozen=True)
class Release(Action):
    b: Block


@dataclass(frozen=True)
class Consider(Action):
    b: Block


@dataclass(frozen=True)
class Continue(Action):
    pass


class PartialView(View):
    def __init__(self, view: View, include: set[Block]):
        self.view = view
        self.include = include

    def children(self, b):
        return self.view.children(b) & self.include

    def parents(self, b):
        return self.view.parents(b) & self.include

    def miner(self, b):
        return self.view.miner(b)


@dataclass(frozen=True, order=True)
class Trace:
    blocks_mined: int
    actions_taken: int


class SelfishMining(Model):
    def __init__(self, editor: StateEditor, config: Config):
        self.editor = editor
        self.config = config
        self.isomorphisms = dict()
        self.symbolic = config.symbolic

        # Use empty state (= no blocks) state as terminal state (sink)
        self.editor.clear()
        self.terminal_state = self.save_or_reuse()

    def save_or_reuse(self, subset=None):
        ic = self.editor.isomorphism_class(subset)
        if ic in self.isomorphisms:
            return self.isomorphisms[ic]
        else:
            binary = self.editor.save()
            self.isomorphisms[ic] = binary
            return binary

    def common_history(self):
        se = self.editor
        protocol = self.config.protocol
        hist_a = []
        hist_b = []
        a = se.preferred_by_defender
        b = se.preferred_by_attacker
        while a is not None:
            hist_a.insert(0, a)
            a = protocol.predecessor(se, a)
        while b is not None:
            hist_b.insert(0, b)
            b = protocol.predecessor(se, b)
        assert hist_a[0] == hist_b[0], "old ca"
        hist_c = []
        while len(hist_a) > 0 and len(hist_b) > 0:
            x = hist_a.pop(0)
            if x == hist_b.pop(0):
                hist_c.append(x)
            else:
                break
        return hist_c

    def start(self) -> TransitionList:
        transitions = []
        # Any genesis we set here will be rewarded in the future. To make this
        # fair, we set two genesis blocks: one mined by the attacker and one by
        # the defender.
        se = self.editor
        cfg = self.config
        tr = Trace(actions_taken=0, blocks_mined=0)
        # prefer attacker
        se.clear()
        b = se.append(set(), mined_by_defender=False)
        se.set_considered_by_attacker(b)
        se.set_released_by_attacker(b)
        se.set_known_to_defender(b)
        se.set_preferred_by_defender(b)
        se.set_preferred_by_attacker(b)
        transitions += self.reward_and_terminate(tr, probability=cfg.alpha)
        # prefer defender
        se.clear()
        b = se.append(set(), mined_by_defender=True)
        se.set_considered_by_attacker(b)
        se.set_known_to_defender(b)
        se.set_preferred_by_defender(b)
        se.set_preferred_by_attacker(b)
        transitions += self.reward_and_terminate(tr, probability=1 - cfg.alpha)
        # return
        return TransitionList(transitions)

    def reward_and_terminate(self, tr: Trace, *args, probability, block_mined=False):
        # TODO. Since we safe progress in Transition now, we can in principle
        # handle stochastic termination AFTER exploration.
        # TODO. We are not using the trace for termination anymore, so we can
        # get rid of it entirely, I guess.
        # increment trace
        tr = Trace(
            blocks_mined=tr.blocks_mined + block_mined,
            actions_taken=tr.actions_taken + 1,
        )
        # find common history
        common_history = self.common_history()
        assert len(common_history) > 0
        common_ancestor = common_history[-1]
        # calculate rewards
        rew_atk = 0.0
        rew_def = 0.0
        for x in common_history[:-1]:
            for r in self.config.protocol.reward(self.editor, x):
                if r.miner == ATTACKER:
                    rew_atk += r.amount
                elif r.miner == DEFENDER:
                    rew_def += r.amount
                else:
                    assert False, "unknown miner"
        # calculate progress
        if len(common_history) > 1:
            pnew = self.config.protocol.progress(self.editor, common_ancestor)
            pold = self.config.protocol.progress(self.editor, common_history[0])
            progress = pnew - pold
            # not sure about the following assertions; currently they hold for
            # all protocols so far
            assert progress > 0
            assert progress == pnew
            assert pold == 0
        else:
            progress = 0
        # stochastic termination
        transitions = []
        if progress > 0:
            if self.symbolic or self.config.horizon > 0.0:
                # Bar-Zur et al. at AFT 2020:
                # Efficient MDP Analysis for Selfish Mining in Blockchains
                H = self.config.horizon
                term_prob = 1.0 - ((1.0 - (1.0 / H)) ** progress)
                assert self.symbolic or (term_prob > 0 and term_prob < 1)
            else:
                # We do this to make the state ids of horizon = 0 model
                # compatible with horizon > 0 model
                term_prob = 0.0
            term = Transition(
                state=self.terminal_state,
                probability=probability * term_prob,
                reward=rew_atk,
                progress=progress,
                trace=tr,
            )
            transitions.append(term)
            probability = probability * (1.0 - term_prob)
        # truncate common history and save
        keep = self.editor.descendants(common_ancestor)
        keep.add(common_ancestor)
        state = self.save_or_reuse(keep)
        # build transition and return
        transitions.append(
            Transition(
                state=state,
                probability=probability,
                trace=tr,
                reward=rew_atk,
                progress=progress,
            )
        )
        return transitions

    def actions(self, s: State) -> list[Action]:
        actions = []
        se = self.editor
        se.load(s)
        # terminal state: no actions possible
        if se.n_blocks == 0:
            return []
        # truncation: allow mining only up to a certain point
        top = self.config.truncate_on_pow
        if top < 1 or se.n_blocks < top:
            actions.append(Continue())
        # release/consider when it makes sense
        for i in se.invert(se.released_by_attacker) - se.mined_by_defender:
            actions.append(Release(i))
        for i in se.invert(se.considered_by_attacker):
            actions.append(Consider(i))
        return actions

    def apply_release(self, b: Block, s: State, tr: Trace) -> TransitionList:
        se = self.editor
        se.load(s)
        # safeguard
        assert b not in se.released_by_attacker, f"block {b} already released"
        assert b not in se.mined_by_defender, f"block {b} cannot be released"
        # mark i and all ancestors as released
        for x in {b} | se.ancestors(b) - se.mined_by_defender:
            se.set_released_by_attacker(x)
        # this transition is deterministic
        transitions = self.reward_and_terminate(tr, probability=1)
        return TransitionList(transitions)

    def apply_consider(self, b: Block, s: State, tr: Trace) -> TransitionList:
        se = self.editor
        cfg = self.config
        se.load(s)
        # safeguard
        assert b not in se.considered_by_attacker, "block {b} already considered"
        # mark i and all ancestors as considered
        for x in {b} | se.ancestors(b):
            se.set_considered_by_attacker(x)
        # update attacker's preference according to protocol spec
        pref = cfg.protocol.preference(
            PartialView(se, se.considered_by_attacker),
            old=se.preferred_by_attacker,
            new=b,
        )
        se.set_preferred_by_attacker(pref)
        # this transition is deterministic
        transitions = self.reward_and_terminate(tr, probability=1)
        return TransitionList(transitions)

    # apply_continue has four cases which we handle individually here
    def _apply_continue(
        self,
        s: State,
        tr: Trace,
        *args,
        attacker_communicates_fast: bool,
        attacker_mines_next_block: bool,
    ) -> list[Transition]:
        se = self.editor
        cfg = self.config
        p = 1
        se.load(s)
        # calculate freshly released blocks
        from_attacker = se.released_by_attacker - se.known_to_defender
        from_attacker = se.topo_sort(from_attacker)
        # calculate freshly mined defender block, if any
        from_defender = se.mined_by_defender - se.known_to_defender
        from_defender = list(from_defender)
        assert len(from_defender) <= 1
        # depending on the case, attacker communicates faster than defender
        if attacker_communicates_fast:
            p *= cfg.gamma
            blocks_received = from_attacker + from_defender
        else:
            p *= 1 - cfg.gamma
            blocks_received = from_defender + from_attacker
        # show blocks to defender, update preference according to protocol spec
        for b in blocks_received:
            se.set_known_to_defender(b)
            pref = cfg.protocol.preference(
                PartialView(se, se.known_to_defender),
                old=se.preferred_by_defender,
                new=b,
            )
            se.set_preferred_by_defender(pref)
        # mine next block
        if attacker_mines_next_block:
            p *= cfg.alpha
            parent_blocks = cfg.protocol.mining(
                PartialView(se, se.considered_by_attacker), se.preferred_by_attacker
            )
            se.append(parent_blocks, mined_by_defender=False)
        else:
            p *= 1 - cfg.alpha
            parent_blocks = cfg.protocol.mining(
                PartialView(se, se.known_to_defender), se.preferred_by_defender
            )
            se.append(parent_blocks, mined_by_defender=True)
        # return Transition
        return self.reward_and_terminate(tr, probability=p, block_mined=True)

    def apply_continue(self, s: State, t: Trace) -> TransitionList:
        lst = []
        for i in [True, False]:
            for j in [True, False]:
                lst += self._apply_continue(
                    s,
                    t,
                    attacker_communicates_fast=i,
                    attacker_mines_next_block=j,
                )
        return TransitionList(lst)

    def apply(self, a: Action, s: State, t: Trace) -> TransitionList:
        # handle action
        if isinstance(a, Release):
            return self.apply_release(a.b, s, t)
        if isinstance(a, Consider):
            return self.apply_consider(a.b, s, t)
        if isinstance(a, Continue):
            return self.apply_continue(s, t)
        assert False, "invalid action"

    def apply_invalid(self, s: State, tr: Trace) -> TransitionList:
        to = Transition(
            state=self.terminal_state,
            probability=1,
            reward=self.config.invalid_reward,
            trace=Trace(
                blocks_mined=tr.blocks_mined, actions_taken=tr.actions_taken + 1
            ),
            progress=0,
        )
        return TransitionList([to])


# Model and protocol spec use generic types which we instantiate here:
# - Block: int
# - State: bytes

N_PROP = 6


class StateEditor(StateEditor):
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
            return DEFENDER
        else:
            return ATTACKER

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
