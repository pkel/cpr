from dataclasses import dataclass
from typing import Union

from protocol import Protocol


@dataclass
class State:
    edges: list[set[int]]  # parent relationship
    attacker_prefers: int
    defender_prefers: int
    ignored_by_attacker: set[int]
    withheld_by_attacker: set[int]
    mined_by_attacker: set[int]
    known_to_defender: set[int]

    def compress(self):
        raise NotImplementedError
        # Truncate common history:
        # 1. Find common ancestor of attacker_prefers and defender_prefers
        # 2. Remove ancestors of common ancestor from state
        # 3. Relabel blocks such that common ancestor becomes node 0
        # 4. Maintain invariance that node are enumerated in order mined


@dataclass
class Release:
    i: int


@dataclass
class Consider:
    i: int


class Continue:
    pass


Action = Union[Release, Consider, Continue]


class DAG:
    def __init__(self, edges: list[set[int]], exclude=set[int]):
        self.parents = edges
        # invert edge list for children relationship
        self.children = [{} for _ in edges]
        child = 0
        for parents in edges:
            for p in parents:
                self.children[p] |= {child}
            child += 1
        # filter excluded blocks
        for edges in self.parents:
            edges -= exclude
        for edges in self.children:
            edges -= exclude


class Block:
    def __init__(self, *args, i: int, dag: DAG):
        self.i = i
        self.dag = dag

    def parents(self):
        return [Block(i, self.dag) for i in self.dag.parents[self.i]]

    def children(self):
        return [Block(i, self.dag) for i in self.dag.children[self.i]]


class Explorer:
    def __init__(self, protocol: Protocol, *args, alpha: float, gamma: float):
        if alpha < 0 or alpha > 1:
            raise ValueError("alpha must be between 0 and 1")
        if gamma < 0 or gamma > 1:
            raise ValueError("gamma must be between 0 and 1")
        self.protocol = Protocol
        self.alpha = alpha
        self.gamma = gamma

    def actions(self, s: State) -> list[Action]:
        lst = [Continue]
        for i in s.withheld_by_attacker:
            lst.append(Release(i))
        for i in s.ignored_by_attacker:
            lst.append(Consider(i))
        return lst

    def release(self, i: int, s: State) -> list[tuple[float, State]]:
        if i not in s.withheld_by_attacker:
            raise ValueError(f"block <{i}> already released")
        s = s.copy()
        # mark i and all ancestors as not withheld
        s.withheld_by_attacker -= {i}
        stack = s.edges[i].copy()
        while len(stack) > 0:
            j = stack.pop()
            if j in s.withheld_by_attacker:
                s.withheld_by_attacker -= {j}
                stack |= s.edges[j]
        # this transition is deterministic
        return [(1, s)]

    def consider(self, i: int, s: State) -> list[tuple[float, State]]:
        if i not in s.ignored_by_attacker:
            raise ValueError(f"block <{i}> not ignored (anymore)")
        s = s.copy()
        # mark i and all ancestors as not ignored
        s.ignored_by_attacker -= {i}
        stack = s.edges[i].copy()
        while len(stack) > 0:
            j = stack.pop()
            if j in s.ignored_by_attacker:
                s.ignored_by_attacker -= {j}
                stack |= s.edges[j]
        # update attacker's preference
        dag = DAG(s.edges, exclude=s.ignored_by_attacker)
        old = Block(s.attacker_prefers, dag)
        new = Block(i, dag)
        s.attacker_prefers = self.protocol.preference(new, old)
        # truncate ancestors of common ancestor
        s.compress()
        # this transition is deterministic
        return [(1, s)]

    def continue_(self, s: State) -> list[tuple[float, State]]:
        # calculate freshly released blocks
        release = s.mined_by_attacker - s.withheld_by_attacker - s.known_to_defender
        release = sorted(list(release))
        # calculate freshly mined defender blocks
        defender = set(range(len(s.edges))) - s.mined_by_defender - s.known_to_defender
        assert len(defender) <= 1
        defender = list(defender)
        # fork two bernoulli outcomes: communication
        # case 1: attacker communicates fast, p = gamma
        s1 = s.copy()
        for i in release + defender:
            s1.known_to_defender |= {i}
            dag = DAG(s1.edges, include=s1.known_to_defender)
            old = Block(s1.attacker_prefers, dag)
            new = Block(i, dag)
            s1.defender_prefers = self.protocol.preference(new, old)
        s1.compress()
        # case 2: attacker communicates slow, p = 1 - gamma
        s2 = s.copy()
        for i in defender + release:
            s2.known_to_defender |= {i}
            dag = DAG(s2.edges, include=s2.known_to_defender)
            old = Block(s2.attacker_prefers, dag)
            new = Block(i, dag)
            s2.defender_prefers = self.protocol.preference(new, old)
        s2.compress()
        transitionsA = [(self.gamma, s1), (1 - self.gamma, s2)]
        # fork two bernoulli outcomes: mining
        transitionsB = []
        for p, s in transitionsA:
            # case 1: attacker mines next block, p = alpha
            s1 = s.copy()
            dag = DAG(s1.edges, exclude=s1.ignored_by_attacker)
            pref = Block(s1.attacker_prefers, dag)
            tmpl = self.protocol.mining(pref)
            # TODO: store template fields in state s1
            n = len(s1.edges)
            s1.edges.append([block.id for block in tmpl.parents])
            s1.withheld_by_attacker |= {n}
            s1.mined_by_attacker |= {n}
            s1.ignored_by_attacker |= {n}
            transitionsB.append((p * self.alpha, s1))
            # case 2: defender mines next block, p = 1 - alpha
            s2 = s.copy()
            dag = DAG(s2.edges, include=s2.known_to_defender)
            pref = Block(s2.defender_prefers, dag)
            tmpl = self.protocol.mining(pref)
            # TODO: store template fields in state s2
            n = len(s2.edges)
            transitionsB.append((p * (1 - self.alpha), s2))
        return transitionsB
