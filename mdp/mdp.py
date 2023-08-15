import copy
import dataclasses
import queue
import numpy as np
import xxhash

import protocol
from protocol import Protocol


class View(protocol.View):
    def __init__(self, state, exclude: set[int] = set(), include: set[int] = None):
        self.state = state
        if include is None:
            self.include = state.all_blocks() - exclude
        else:
            self.include = include - exclude

    def children(self, b):
        return self.state.children[b] & self.include

    def parents(self, b):
        return self.state.parents[b] & self.include

    def miner(self, b):
        if b in self.mined_by_attacker:
            return 0
        else:
            return 1


@dataclasses.dataclass()
class State:
    # TODO encode this closer to packed state, i.e. single np.array
    children: list[set[int]]
    parents: list[set[int]]
    attacker_prefers: int
    defender_prefers: int
    ignored_by_attacker: set[int]
    withheld_by_attacker: set[int]
    mined_by_attacker: set[int]
    known_to_defender: set[int]

    def compress(self, protocol: Protocol):
        # Truncate common history:
        # 1. Find common ancestor of attacker_prefers and defender_prefers
        # 2. Filter blocks:
        #     - remove ancestors of common ancestor
        #     - keep ancestors/descendants of attacker_prefers/defender_prefers
        #     - throw out the rest: descendants of common ancestors which are
        #       not predecessor of attacker_prefers/defender_prefers
        # 3. Deterministically label blocks by enumerating blocks in
        #    topological order.
        # 4. Avoid graph isomorphisms TODO

        parents = self.parents
        children = self.children
        view = View(self)

        def predecessor(b):
            return protocol.predecessor(view, b)

        # find common ancestor
        a = self.defender_prefers
        b = self.attacker_prefers
        while a != b:
            if a < b:
                b = predecessor(b)
            else:
                a = predecessor(a)
        ca = a

        # keep blocks reachable from attacker/defender preference
        keep = set()
        for entry in [self.attacker_prefers, self.defender_prefers]:
            keep.add(entry)
            # ancestors
            todo = parents[entry].copy()
            while len(todo) > 0:
                i = todo.pop()
                keep.add(i)
                todo |= parents[i]
            # descendants
            todo = children[entry].copy()
            while len(todo) > 0:
                i = todo.pop()
                keep.add(i)
                todo |= children[i]
        # remove ancestors of common ancestor
        todo = parents[ca].copy()
        while len(todo) > 0:
            i = todo.pop()
            keep.remove(i)
            todo |= parents[i]

        # relabel blocks in topological order
        id_map = dict()
        todo = {ca}
        while len(todo) > 0:
            old_id = todo.pop()
            if old_id in keep:
                todo |= children[old_id]
                assert old_id not in id_map
                new_id = len(id_map)
                id_map[old_id] = new_id
        # TODO. children is a set and its order ambiguous. Deterministic order
        # could avoid isomorphisms.

        # print()
        # print("pre-compress", self)
        # print("parents", parents)
        # print("children", children)
        # print("ca", ca)
        # print("keep", keep)
        # print("id_map", id_map)

        # create new state
        new_parents = [set()] * len(id_map)
        new_children = [set()] * len(id_map)
        for old_id, new_id in id_map.items():
            p = self.parents[old_id]
            new_parents[new_id] = {id_map[x] for x in p if x in id_map}
            c = self.children[old_id]
            new_children[new_id] = {id_map[x] for x in c if x in id_map}
        self.children = new_children
        self.parents = new_parents
        self.attacker_prefers = id_map[self.attacker_prefers]
        self.defender_prefers = id_map[self.defender_prefers]

        def compress_set(s):
            return {id_map[x] for x in s if x in id_map}

        self.withheld_by_attacker = compress_set(self.withheld_by_attacker)
        self.mined_by_attacker = compress_set(self.mined_by_attacker)
        self.known_to_defender = compress_set(self.known_to_defender)
        self.ignored_by_attacker = compress_set(self.ignored_by_attacker)

        # print("post-compress", self)

        return None

    def all_blocks(self):
        return set(range(len(self.parents)))

    def invert_blockset(self, s: set[int]):
        return self.all_blocks() - s

    def append(self, parents: set[int]) -> int:
        id = len(self.parents)
        self.parents.append(parents)
        self.children.append(set())
        for p in parents:
            self.children[p].add(id)
        return id

    def copy(self):
        # TODO avoid deepcopy of state. Do repeated unpacks instead
        return copy.deepcopy(self)

    def digest(self):
        data = []
        data.append(self.parents)
        #  data.append(self.children)  # redundant with parents
        data.append(self.attacker_prefers)
        data.append(self.defender_prefers)
        data.append(sorted(list(self.withheld_by_attacker)))
        data.append(sorted(list(self.known_to_defender)))
        data.append(sorted(list(self.ignored_by_attacker)))
        data.append(sorted(list(self.mined_by_attacker)))
        return xxhash.xxh3_128_digest(repr(data))

    def pack(self):
        n = len(self.parents)
        rows = self.parents.copy()
        rows.append(self.ignored_by_attacker)
        rows.append(self.withheld_by_attacker)
        rows.append(self.mined_by_attacker)
        rows.append(self.known_to_defender)
        rows.append({self.attacker_prefers})
        rows.append({self.defender_prefers})
        a = np.full((n + 6, n), False)
        for i, s in enumerate(rows):
            for x in s:
                a[i, x] = True
        packed_rows = np.packbits(a, axis=None)
        return np.append(packed_rows, [np.uint8(n)])


def unpack(packed):
    n = packed[-1]
    size = (n + 6) * n
    packed_rows = packed[:-1]
    a = np.unpackbits(packed_rows, axis=None)[:size].reshape((n + 6, n)).view(bool)
    rows = []
    for i in range(n + 6):
        rows.append(set())
        for x in range(n):
            if a[i, x]:
                rows[i].add(x)
    parents = rows[:n]
    iba = rows[n]
    wba = rows[n + 1]
    mba = rows[n + 2]
    ktd = rows[n + 3]
    ap = rows[n + 4]
    dp = rows[n + 5]
    assert len(ap) == 1
    assert len(dp) == 1
    children = [set() for _ in range(n)]
    for i, s in enumerate(parents):
        for p in s:
            children[p].add(i)
    return State(
        parents=parents,
        children=children,
        attacker_prefers=ap.pop(),
        defender_prefers=dp.pop(),
        ignored_by_attacker=iba,
        withheld_by_attacker=wba,
        mined_by_attacker=mba,
        known_to_defender=ktd,
    )


@dataclasses.dataclass
class Config:
    alpha: float
    gamma: float
    protocol: Protocol

    def __post_init__(self):
        if self.alpha < 0 or self.alpha > 1:
            raise ValueError("alpha must be between 0 and 1")
        if self.gamma < 0 or self.gamma > 1:
            raise ValueError("gamma must be between 0 and 1")


@dataclasses.dataclass
class Transition:
    prob: float
    state: State
    timestep: bool
    # TODO. reward: float


class Action:
    def apply(self, s: State, cfg: Config) -> list[Transition]:
        raise NotImplementedError


@dataclasses.dataclass(frozen=True)
class Release(Action):
    i: int

    def apply(self, s: State, cfg: Config) -> list[Transition]:
        if self.i not in s.withheld_by_attacker:
            raise ValueError(f"block <{self.i}> already released")
        s = s.copy()  # TODO avoid deepcopy; do unpack, modify, pack
        # mark i and all ancestors as not withheld
        s.withheld_by_attacker -= {self.i}
        todo = s.parents[self.i].copy()
        while len(todo) > 0:
            j = todo.pop()
            if j in s.withheld_by_attacker:
                s.withheld_by_attacker -= {j}
                todo |= s.parents[j]
        # this transition is deterministic
        return [Transition(1, s, False)]


@dataclasses.dataclass(frozen=True)
class Consider(Action):
    i: int

    def apply(self, s: State, cfg: Config) -> list[Transition]:
        if self.i not in s.ignored_by_attacker:
            raise ValueError(f"block <{self. i}> not ignored (anymore)")
        s = s.copy()  # TODO avoid deepcopy; do unpack, modify, pack
        # mark i and all ancestors as not ignored
        s.ignored_by_attacker -= {self.i}
        todo = s.parents[self.i].copy()
        while len(todo) > 0:
            j = todo.pop()
            if j in s.ignored_by_attacker:
                s.ignored_by_attacker.remove(j)
                todo |= s.parents[j]
        # update attacker's preference
        s.attacker_prefers = cfg.protocol.preference(
            View(s, exclude=s.ignored_by_attacker),
            old=s.attacker_prefers,
            new=self.i,
        )
        # truncate ancestors of common ancestor
        s.compress(cfg.protocol)
        # this transition is deterministic
        return [Transition(1, s, False)]


@dataclasses.dataclass(frozen=True)
class Continue(Action):
    def apply(self, s: State, cfg: Config) -> list[Transition]:
        # calculate freshly released blocks
        release = s.mined_by_attacker - s.withheld_by_attacker - s.known_to_defender
        release = sorted(list(release))
        # calculate freshly mined defender blocks
        defender = s.all_blocks() - s.mined_by_attacker - s.known_to_defender
        assert len(defender) <= 1
        defender = list(defender)
        # fork two bernoulli outcomes: communication
        # case 1: attacker communicates fast, p = gamma
        s1 = s.copy()  # TODO avoid deepcopy; do unpack, modify, pack
        for i in release + defender:
            s1.known_to_defender.add(i)
            s1.defender_prefers = cfg.protocol.preference(
                View(s1, include=s1.known_to_defender),
                old=s1.defender_prefers,
                new=i,
            )
        s1.compress(cfg.protocol)
        # case 2: attacker communicates slow, p = 1 - gamma
        s2 = s.copy()  # TODO avoid deepcopy; do unpack, modify, pack
        for i in defender + release:
            s2.known_to_defender.add(i)
            s2.defender_prefers = cfg.protocol.preference(
                View(s2, include=s2.known_to_defender),
                old=s2.defender_prefers,
                new=i,
            )
        s2.compress(cfg.protocol)
        transitionsA = [(cfg.gamma, s1), (1 - cfg.gamma, s2)]
        # fork two bernoulli outcomes: mining
        transitionsB = []
        for p, s in transitionsA:
            # case 1: attacker mines next block, p = alpha
            s1 = s.copy()  # TODO avoid deepcopy; do unpack, modify, pack
            view = View(s1, exclude=s1.ignored_by_attacker)
            append_to = cfg.protocol.mining(view, s1.attacker_prefers)
            new_b = s1.append(append_to)
            s1.withheld_by_attacker |= {new_b}
            s1.mined_by_attacker |= {new_b}
            transitionsB.append((p * cfg.alpha, s1))
            # case 2: defender mines next block, p = 1 - alpha
            s2 = s.copy()  # TODO avoid deepcopy; do unpack, modify, pack
            view = View(s2, include=s2.known_to_defender)
            append_to = cfg.protocol.mining(view, s2.defender_prefers)
            new_b = s2.append(append_to)
            s2.ignored_by_attacker |= {new_b}
            transitionsB.append((p * (1 - cfg.alpha), s2))
        return [Transition(p, s, True) for p, s in transitionsB]


@dataclasses.dataclass(order=True)
class QState:
    distance_time: int
    distance_step: int
    id: int
    digest: bytes
    packed_state: np.ndarray = dataclasses.field(compare=False)


class Explorer:
    def __init__(self, cfg: Config):
        self.config = cfg
        self.queue = queue.PriorityQueue()
        self.action_map = dict()  # maps action to integer
        self.state_map = dict()  # maps state digest to integer
        self.states_explored = 0
        self.max_actions = 0
        self.reset_transitions = 0
        self.nonreset_transitions = 0
        # handle start state
        state = self.start_state()
        digest = state.digest()
        self.state_map[digest] = 0
        self.queue.put(QState(0, 0, 0, digest, state.pack()))

    def start_state(self):
        start = State(
            children=[set()],
            parents=[set()],
            attacker_prefers=0,
            defender_prefers=0,
            ignored_by_attacker=set(),
            withheld_by_attacker=set(),
            mined_by_attacker=set(),
            known_to_defender={0},
        )
        return start

    def step(self):
        src = self.queue.get()
        self.states_explored += 1
        src_state = unpack(src.packed_state)

        actions = self.actions(src_state)
        self.max_actions = max(self.max_actions, len(actions))
        # TODO max_actions is about factor two smaller than action_map. By
        # renaming the actions per source state, we can cut the number of
        # actions in half.

        for action in actions:
            # alias action/integer
            if action in self.action_map:
                action_id = self.action_map[action]
            else:
                action_id = len(self.action_map)
                self.action_map[action] = action_id

            # apply action, obtain probabilistic transitions
            for t in action.apply(src_state, self.config):
                # alias state/digest/integer
                dst_digest = t.state.digest()
                if dst_digest in self.state_map:
                    dst_id = self.state_map[dst_digest]
                else:
                    # we see this state for the first time
                    dst_id = len(self.state_map)
                    self.state_map[dst_digest] = dst_id
                    # double check state packing
                    dst_packed = t.state.pack()
                    # TODO this safeguard slows down exploration.
                    # Move into unit test!
                    assert unpack(dst_packed).digest() == dst_digest
                    # recursive exploration
                    dst = QState(
                        distance_time=src.distance_time + t.timestep,
                        distance_step=src.distance_step + 1,
                        id=dst_id,
                        digest=dst_digest,
                        packed_state=dst_packed,
                    )
                    self.queue.put(dst)

                # sanity check; we do not have noop actions
                assert src.id != dst_id

                # statistics
                if dst_id == 0:
                    self.reset_transitions += 1
                else:
                    self.nonreset_transitions += 1

                # record transition
                # TODO

    def actions(self, s: State) -> list[Action]:
        lst = [Continue()]
        for i in s.withheld_by_attacker:
            lst.append(Release(i))
        for i in s.ignored_by_attacker:
            lst.append(Consider(i))
        return lst

    def explore(self, steps=1000) -> bool:
        for i in range(steps):
            if self.queue.empty():
                return False
            else:
                self.step()

    def peek(self):
        s = self.queue.get()
        self.queue.put(s)
        return s
