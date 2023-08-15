import copy
import dataclasses
import queue
from typing import Union
import xxhash

from protocol import Protocol, Template


@dataclasses.dataclass
class State:
    edges: list[list[int]]  # parent relationship
    blocks: list[dict[str, Union[int, float, str]]]
    attacker_prefers: int
    defender_prefers: int
    ignored_by_attacker: set[int]
    withheld_by_attacker: set[int]
    mined_by_attacker: set[int]
    known_to_defender: set[int]

    def compress(self):
        # Truncate common history:
        # 1. Find common ancestor of attacker_prefers and defender_prefers
        # 2. Filter blocks:
        #     - remove ancestors of common ancestor
        #     - keep ancestors/descendants of attacker_prefers/defender_prefers
        #     - throw out the rest: descendants of common ancestors which are
        #       not predecessor of attacker_prefers/defender_prefers
        # 3. Deterministically label blocks by enumerating blocks in
        #    topological order.

        # TODO I think step 2.) above implies restrictions to the protocol spec
        # API. Preference and mining may use children but not parents.
        # Predecessor and reward may use parents but not children.

        # TODO To further shrink the state space it might be worth to change
        # the type of edges to list[set[int]], but this would require changes
        # to the protocol specification API.

        # TODO. Use protocol's predecessor.
        # As is, it will only work for Nakamoto/Bitcoin.
        def predecessor(i):
            return self.edges[i][0]

        parents = self.edges
        children = self.children()

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
            todo = set(parents[entry])
            while len(todo) > 0:
                i = todo.pop()
                keep.add(i)
                todo |= set(parents[i])
            # descendants
            todo = children[entry].copy()
            while len(todo) > 0:
                i = todo.pop()
                keep.add(i)
                todo |= children[i]
        # remove ancestors of common ancestor
        todo = set(parents[ca])
        while len(todo) > 0:
            i = todo.pop()
            keep.remove(i)
            todo |= set(parents[i])

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
        # TODO. children returns set and is ambiguous. We need a deterministic
        # ordering of children(). Alternatively, we can maybe locate the leaves
        # and use the parents relationship. Parents are ordered, so we only
        # have to order the leaves deterministically.

        # print()
        # print("pre-compress", self)
        # print("parents", parents)
        # print("children", children)
        # print("ca", ca)
        # print("keep", keep)
        # print("id_map", id_map)

        # create new state
        new_edges = [[]] * len(id_map)
        new_blocks = [dict()] * len(id_map)
        for old_id, new_id in id_map.items():
            e = self.edges[old_id]
            new_edges[new_id] = [id_map[x] for x in e if x in id_map]
            new_blocks[new_id] = self.blocks[old_id]
        self.edges = new_edges
        self.attacker_prefers = id_map[self.attacker_prefers]
        self.defender_prefers = id_map[self.defender_prefers]

        def compress_set(s):
            return {id_map[x] for x in s if x in id_map}

        self.withheld_by_attacker = compress_set(self.withheld_by_attacker)
        self.mined_by_attacker = compress_set(self.mined_by_attacker)
        self.known_to_defender = compress_set(self.known_to_defender)
        self.ignored_by_attacker = compress_set(self.ignored_by_attacker)

        #  print("post-compress", self)

        return None

    def all_blocks(self):
        return set(range(len(self.edges)))

    def invert_blockset(self, s: set[int]):
        return self.all_blocks() - s

    def append_template(self, template: Template):
        i = len(self.edges)
        d = dataclasses.asdict(template)
        p = [x.i for x in d.pop("parents")]
        self.edges.append(p)
        self.blocks.append(d)
        return i

    def children(self):
        # invert parent relationship
        children = [set() for _ in self.edges]
        for block, parents in enumerate(self.edges):
            for p in parents:
                children[p].add(block)
        return children

    def copy(self):
        return copy.deepcopy(self)

    def digest(self):
        data = []
        data.append(self.edges)
        #  data.append(self.blocks)  # intentionally not hashing block data
        data.append(self.attacker_prefers)
        data.append(self.defender_prefers)
        data.append(sorted(list(self.withheld_by_attacker)))
        data.append(sorted(list(self.mined_by_attacker)))
        data.append(sorted(list(self.known_to_defender)))
        data.append(sorted(list(self.ignored_by_attacker)))
        return xxhash.xxh3_128_digest(repr(data))


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
        s = s.copy()
        # mark i and all ancestors as not withheld
        s.withheld_by_attacker -= {self.i}
        stack = set(s.edges[self.i])
        while len(stack) > 0:
            j = stack.pop()
            if j in s.withheld_by_attacker:
                s.withheld_by_attacker -= {j}
                stack |= set(s.edges[j])
        # this transition is deterministic
        return [Transition(1, s, False)]


@dataclasses.dataclass(frozen=True)
class Consider(Action):
    i: int

    def apply(self, s: State, cfg: Config) -> list[Transition]:
        if self.i not in s.ignored_by_attacker:
            raise ValueError(f"block <{self. i}> not ignored (anymore)")
        s = s.copy()
        # mark i and all ancestors as not ignored
        s.ignored_by_attacker -= {self.i}
        stack = set(s.edges[self.i])
        while len(stack) > 0:
            j = stack.pop()
            if j in s.ignored_by_attacker:
                s.ignored_by_attacker -= {j}
                stack |= set(s.edges[j])
        # update attacker's preference
        dag = DAG(s, exclude=s.ignored_by_attacker)
        old = Block(s.attacker_prefers, dag)
        new = Block(self.i, dag)
        s.attacker_prefers = cfg.protocol.preference(old, new).i
        # truncate ancestors of common ancestor
        s.compress()
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
        s1 = s.copy()
        for i in release + defender:
            s1.known_to_defender |= {i}
            dag = DAG(s1, include=s1.known_to_defender)
            old = Block(s1.defender_prefers, dag)
            new = Block(i, dag)
            s1.defender_prefers = cfg.protocol.preference(old, new).i
        s1.compress()
        # case 2: attacker communicates slow, p = 1 - gamma
        s2 = s.copy()
        for i in defender + release:
            s2.known_to_defender |= {i}
            dag = DAG(s2, include=s2.known_to_defender)
            old = Block(s2.defender_prefers, dag)
            new = Block(i, dag)
            s2.defender_prefers = cfg.protocol.preference(old, new).i
        s2.compress()
        transitionsA = [(cfg.gamma, s1), (1 - cfg.gamma, s2)]
        # fork two bernoulli outcomes: mining
        transitionsB = []
        for p, s in transitionsA:
            # case 1: attacker mines next block, p = alpha
            s1 = s.copy()
            dag = DAG(s1, exclude=s1.ignored_by_attacker)
            pref = Block(s1.attacker_prefers, dag)
            tmpl = cfg.protocol.mining(pref)
            b = s1.append_template(tmpl)
            s1.withheld_by_attacker |= {b}
            s1.mined_by_attacker |= {b}
            transitionsB.append((p * cfg.alpha, s1))
            # case 2: defender mines next block, p = 1 - alpha
            s2 = s.copy()
            s2.ignored_by_attacker |= {b}
            dag = DAG(s2, include=s2.known_to_defender)
            pref = Block(s2.defender_prefers, dag)
            tmpl = cfg.protocol.mining(pref)
            _ = s2.append_template(tmpl)
            transitionsB.append((p * (1 - cfg.alpha), s2))
        return [Transition(p, s, True) for p, s in transitionsB]


class DAG:
    def __init__(
        self, state: State, exclude: set[int] = set(), include: set[int] = None
    ):
        self.state = state
        if include is None:
            self.include = state.all_blocks() - exclude
        else:
            self.include = include - exclude

        self._children = None

    @property
    def children(self):
        if self._children is None:
            self._children = self.state.children()
        return self._children


class Block:
    def __init__(self, i: int, dag: DAG):
        self.i = i
        self.dag = dag

    def parents(self):
        filtered = [i for i in self.dag.state.edges[self.i] if i in self.include]
        return [Block(i, self.dag) for i in filtered]

    def children(self):
        filtered = [i for i in self.dag.children[self.i] if i in self.include]
        return [Block(i, self.dag) for i in filtered]
        # This would be easier with an adjacency matrix
        raise NotImplementedError

    def __getattr__(self, name):
        if name.startswith("_"):
            raise AttributeError(name)
        if name in self.dag.state.blocks[self.i]:
            return self.dag.state.blocks[self.i][name]
        else:
            raise AttributeError(name)


@dataclasses.dataclass(order=True)
class QState:
    distance_time: int
    distance_step: int
    id: int
    digest: bytes
    state: State = dataclasses.field(compare=False)


class Explorer:
    def __init__(self, cfg: Config):
        self.config = cfg
        self.queue = queue.PriorityQueue()
        self.action_map = dict()  # maps action to integer
        self.state_map = dict()  # maps state digest to integer
        self.states_explored = 0
        self.max_actions = 0
        # handle start state
        state = self.start_state()
        digest = state.digest()
        self.state_map[digest] = 0
        self.queue.put(QState(0, 0, 0, digest, state))

    def start_state(self):
        start = State(
            edges=[],
            blocks=[],
            attacker_prefers=0,
            defender_prefers=0,
            ignored_by_attacker=set(),
            withheld_by_attacker=set(),
            mined_by_attacker=set(),
            known_to_defender={0},
        )
        g = start.append_template(self.config.protocol.genesis())
        assert g == 0
        return start

    def step(self):
        src = self.queue.get()
        self.states_explored += 1

        actions = self.actions(src.state)
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
            for t in action.apply(src.state, self.config):
                # alias state/digest/integer
                dst_digest = t.state.digest()
                if dst_digest in self.state_map:
                    dst_id = self.state_map[dst_digest]
                else:
                    # we see this state for the first time
                    dst_id = len(self.state_map)
                    self.state_map[dst_digest] = dst_id
                    # recursive exploration
                    dst = QState(
                        distance_time=src.distance_time + t.timestep,
                        distance_step=src.distance_step + 1,
                        id=dst_id,
                        digest=dst_digest,
                        state=t.state,
                    )
                    self.queue.put(dst)

                # sanity check; we do not have noop actions
                assert src.id != dst_id

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
