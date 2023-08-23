from enum import IntEnum
from dataclasses import dataclass
from model import Model, Transition, TransitionList
from protocol import Protocol, View
import numpy
import pickle

Miner = IntEnum("Miner", ["Attacker", "Defender"])

AttackerView = IntEnum("AttackerView", ["Ignored", "Considered", "Preferred"])
DefenderView = IntEnum("DefenderView", ["Unknown", "Known", "Preferred"])
Withholding = IntEnum("Withholding", ["Foreign", "Withheld", "Released"])

State = bytes


@dataclass(frozen=True)
class Action:
    pass


@dataclass(frozen=True)
class Release(Action):
    i: int


@dataclass(frozen=True)
class Consider(Action):
    i: int


@dataclass(frozen=True)
class Continue(Action):
    pass


class Editor(View):
    def __init__(self, *args, first_miner: Miner):
        self.n = 1
        self.adj = numpy.full((1, 1), False)
        self.av = [AttackerView.Preferred]
        self.dv = [DefenderView.Preferred]
        if first_miner == Miner.Attacker:
            self.wh = [Withholding.Withheld]
        if first_miner == Miner.Defender:
            self.wh = [Withholding.Foreign]
        assert isinstance(first_miner, Miner)
        self.check()

    def save(self):
        self.check()
        return pickle.dumps(self)

    def load(self, buf):
        x = pickle.loads(buf)
        self.n = x.n
        self.adj = x.adj
        self.av = x.av
        self.dv = x.dv
        self.wh = x.wh
        self.check()

    def check(self):
        # shape and lengths
        assert self.adj.shape == (self.n, self.n)
        assert len(self.av) == self.n
        assert len(self.dv) == self.n
        assert len(self.wh) == self.n
        # exactly one preferred
        apref = 0
        dpref = 0
        for i in range(self.n):
            if self.av[i] == AttackerView.Preferred:
                apref += 1
            if self.dv[i] == DefenderView.Preferred:
                dpref += 1
        assert apref == 1, f"{apref} blocks preferred by attacker"
        assert dpref == 1, f"{dpref} blocks preferred by defender"
        # types
        for i in range(self.n):
            isinstance(self.av[i], AttackerView)
            isinstance(self.dv[i], DefenderView)
            isinstance(self.wh[i], Withholding)
        # topological order
        for b in range(self.n):
            for p in self.parents(b):
                assert p < b, "topological order"
        # exactly one genesis
        no_parents_cnt = 0
        for b in range(self.n):
            if len(self.parents(b)) == 0:
                no_parents_cnt += 1
        assert no_parents_cnt == 1, f"{no_parents_cnt} root blocks"

    def parents(self, b):
        return {int(x) for x in numpy.flatnonzero(self.adj[b, 0 : self.n])}

    def children(self, b):
        return {int(x) for x in numpy.flatnonzero(self.adj[0 : self.n, b])}

    def miner(self, b):
        if self.wh[b] == Withholding.Foreign:
            return Miner.Defender
        else:
            return Miner.Attacker

    def append(self, parents: set[int], miner: Miner):
        assert isinstance(miner, Miner)
        assert isinstance(parents, set)
        for p in parents:
            assert isinstance(p, int), f"p = {p}"

        b = self.n
        # construct new adjacency matrix
        new_n = self.n + 1
        new_adj = numpy.full((new_n, new_n), False)
        new_adj[0 : self.n, 0 : self.n] = self.adj
        for p in parents:
            new_adj[b, p] = True
        # update editor in place
        self.n = new_n
        self.adj = new_adj
        self.av.append(AttackerView.Ignored)
        self.dv.append(DefenderView.Unknown)
        if miner == Miner.Attacker:
            self.wh.append(Withholding.Withheld)
        elif miner == Miner.Defender:
            self.wh.append(Withholding.Foreign)
        else:
            assert False, "unkown miner"
        # safety check
        self.check()
        assert isinstance(b, int)
        return b

    def reorder_and_filter(self, old_ids: list[int]):
        assert isinstance(old_ids, list)
        assert len(old_ids) > 0
        assert len(old_ids) == len(set(old_ids))

        # recall old state
        old_adj = self.adj
        old_av = self.av
        old_dv = self.dv
        old_wh = self.wh

        # update in-place
        self.n = len(old_ids)
        self.adj = numpy.full((self.n, self.n), False)
        self.av = []
        self.dv = []
        self.wh = []

        # copy stuff in new order
        for new, old in enumerate(old_ids):
            self.adj[new, 0 : self.n] = old_adj[old, old_ids]
            self.av.append(old_av[old])
            self.dv.append(old_dv[old])
            self.wh.append(old_wh[old])

        # safety check
        self.check()

    def to_release(self):
        # withheld blocks w/o withheld parents
        lst = []
        for b in range(self.n):
            if self.wh[b] == Withholding.Withheld:
                parents_ok = True
                for p in self.parents(b):
                    if self.wh[p] == Withholding.Withheld:
                        parents_ok = False
                        continue
                if parents_ok:
                    lst.append(b)
        return lst

    def to_consider(self):
        # ignored blocks w/o ignored parents
        lst = []
        for b in range(self.n):
            assert isinstance(self.av[b], AttackerView)
            if self.av[b] == AttackerView.Ignored:
                parents_ok = True
                for p in self.parents(b):
                    if self.av[p] == AttackerView.Ignored:
                        parents_ok = False
                        continue
                if parents_ok:
                    lst.append(b)
        return lst

    def just_released(self):
        # released but unknown blocks
        lst = []
        for b in range(self.n):
            if self.dv[b] == DefenderView.Unknown:
                if self.wh[b] == Withholding.Released:
                    lst.append(b)
        return lst

    def just_mined_by_defender(self):
        # defender blocks which are still unknown
        lst = []
        for b in range(self.n):
            if self.dv[b] == DefenderView.Unknown:
                if self.wh[b] == Withholding.Foreign:
                    lst.append(b)
        return lst

    def attacker_view(self):
        # attacker only sees not ignored blocks
        return PartialView(self, lambda b: self.av[b] != AttackerView.Ignored)

    def defender_view(self):
        # defender only sees not unknown blocks
        return PartialView(self, lambda b: self.dv[b] != DefenderView.Unknown)

    def attacker_prefers(self):
        for b in range(self.n):
            if self.av[b] == AttackerView.Preferred:
                return b
        assert False, "no pref"

    def defender_prefers(self):
        for b in range(self.n):
            if self.dv[b] == DefenderView.Preferred:
                return b
        assert False, "no pref"

    def set_released(self, b):
        assert self.wh[b] == Withholding.Withheld
        for p in self.parents(b):
            assert self.wh[p] != Withholding.Withheld
        self.wh[b] = Withholding.Released

    def set_known(self, b):
        assert self.dv[b] == DefenderView.Unknown
        for p in self.parents(b):
            assert self.dv[p] != DefenderView.Unknown
        self.dv[b] = DefenderView.Known

    def set_considered(self, b):
        assert self.av[b] == AttackerView.Ignored
        for p in self.parents(b):
            assert self.av[p] != AttackerView.Ignored
        self.av[b] = AttackerView.Considered

    def set_defender_prefers(self, b):
        assert self.dv[b] != DefenderView.Unknown
        for i in range(self.n):
            if self.dv[i] == DefenderView.Preferred:
                self.dv[i] = DefenderView.Known
        self.dv[b] = DefenderView.Preferred

    def set_attacker_prefers(self, b):
        assert self.av[b] != AttackerView.Ignored
        for i in range(self.n):
            if self.av[i] == AttackerView.Preferred:
                self.av[i] = AttackerView.Considered
        self.av[b] = AttackerView.Preferred


class PartialView(View):
    def __init__(self, view: View, filter=lambda _: True):
        # check argument; first block is always visible
        assert filter(0)

        self.view = view
        self.filter = filter

    def children(self, b):
        assert self.filter(b)
        return {b for b in self.view.children(b) if self.filter(b)}

    def parents(self, b):
        assert self.filter(b)

        parents = self.view.parents(b)
        for p in parents:
            assert self.filter(b)
        return parents

    def miner(self, b):
        return self.view.miner(b)


@dataclass(order=True)
class Trace:
    pass


class SelfishMining(Model):
    def __init__(
        self,
        protocol: Protocol,
        *args,
        alpha: float,
        gamma: float,
        maximum_size: int,
        force_consider_own: bool = True,
    ):
        assert isinstance(protocol, Protocol)
        assert alpha >= 0 and alpha <= 1
        assert gamma >= 0 and gamma <= 1
        assert maximum_size > 0
        self.alpha = alpha
        self.gamma = gamma
        self.maximum_size = maximum_size
        self.force_consider_own = force_consider_own
        self.protocol = protocol

        self.editor = Editor(first_miner=Miner.Attacker)

        # we don't use the trace, just return this one everywhere
        self.trace = Trace()

    def __repr__(self):
        return (
            f"SelfishMining({self.protocol.name}, "
            f"alpha={self.alpha}, "
            f"gamma={self.gamma}, "
            f"maximum_size={self.maximum_size}, "
            f"force_consider_own={self.force_consider_own})"
        )

    def common_history(self):
        e = self.editor
        hist_a = []
        hist_b = []
        a = e.attacker_prefers()
        b = e.defender_prefers()
        while a is not None:
            hist_a.insert(0, a)
            a = self.protocol.predecessor(e, a)
        while b is not None:
            hist_b.insert(0, b)
            b = self.protocol.predecessor(e, b)
        assert hist_a[0] == hist_b[0], "old ca"
        hist_c = []
        while len(hist_a) > 0 and len(hist_b) > 0:
            x = hist_a.pop(0)
            if x == hist_b.pop(0):
                hist_c.append(x)
            else:
                break
        return hist_c

    def transition(self, *args, probability):
        # TODO: consider merging isomorphic states

        e = self.editor

        # find common history
        common_history = self.common_history()
        assert len(common_history) > 0
        common_ancestor = common_history[-1]

        # calculate rewards
        rew_atk = 0.0
        rew_def = 0.0
        for x in common_history[:-1]:
            for r in self.protocol.reward(e, x):
                if r.miner == Miner.Attacker:
                    rew_atk += r.amount
                elif r.miner == Miner.Defender:
                    rew_def += r.amount
                else:
                    assert False, "unknown miner"

        # calculate progress
        if len(common_history) > 1:
            pnew = self.protocol.progress(e, common_ancestor)
            pold = self.protocol.progress(e, common_history[0])
            progress = pnew - pold
            # not sure about the following assertions; they hold for
            # bitcoin and parallel as implemented currently
            assert progress > 0
            assert progress == pnew
            assert pold == 0
        else:
            progress = 0

        # truncate common history
        keep = {common_ancestor} | self.editor.descendants(common_ancestor)
        e.reorder_and_filter(sorted(list(keep)))
        # TODO consider throwing out blocks that cannot be reached from
        # attacker's and defender's preference

        return Transition(
            state=self.editor.save(),
            probability=probability,
            progress=progress,
            reward=rew_atk,
            trace=self.trace,
        )

    def start(self) -> TransitionList:
        lst = []
        self.editor = Editor(first_miner=Miner.Attacker)
        lst.append(self.transition(probability=self.alpha))
        self.editor = Editor(first_miner=Miner.Defender)
        lst.append(self.transition(probability=1 - self.alpha))
        return TransitionList(lst)

    def actions(self, s: State) -> list[Action]:
        actions = []
        e = self.editor
        e.load(s)

        # truncation: allow mining only up to a certain point
        ms = self.maximum_size
        if ms < 1 or e.n < ms:
            actions.append(Continue())

        # release/consider when it makes sense
        for i, _ in enumerate(e.to_release()):
            actions.append(Release(i))
        for i, _ in enumerate(e.to_consider()):
            actions.append(Consider(i))

        return actions

    def apply(self, a: Action, s: State, t: Trace) -> TransitionList:
        if isinstance(a, Release):
            return self.apply_release(a.i, s)
        if isinstance(a, Consider):
            return self.apply_consider(a.i, s)
        if isinstance(a, Continue):
            return self.apply_continue(s)
        assert isinstance(a, Action)
        assert False, "unknown action"

    def apply_release(self, i: int, s: State) -> TransitionList:
        e = self.editor
        e.load(s)
        # which block will be released?
        b = e.to_release()[i]
        # mark b as released
        e.set_released(b)
        # this transition is deterministic
        lst = [self.transition(probability=1)]
        return TransitionList(lst)

    def apply_consider(self, i: int, s: State) -> TransitionList:
        e = self.editor
        e.load(s)
        # which block will be considered?
        b = e.to_consider()[i]
        # mark b as considered
        e.set_considered(b)
        # update attacker's preference
        pref = self.protocol.preference(
            e.attacker_view(),
            old=e.attacker_prefers(),
            new=b,
        )
        e.set_attacker_prefers(pref)
        # this transition is deterministic
        lst = [self.transition(probability=1)]
        return TransitionList(lst)

    def apply_continue(self, s: State) -> TransitionList:
        lst = []
        for gamma in [True, False]:
            for alpha in [True, False]:
                lst.append(self._apply_continue(s, gamma, alpha))
        return TransitionList(lst)

    def _apply_continue(self, s: State, gamma: bool, alpha: bool):
        e = self.editor
        e.load(s)
        prob = 1.0
        # blocks just released by attacker
        from_attacker = e.just_released()
        # blocks just mined by defender
        from_defender = e.just_mined_by_defender()
        assert len(from_defender) <= 1
        # depending on the case, attacker blocks arrive early
        if gamma:  # attacker communicates fast
            prob *= self.gamma
            deliver = from_attacker + from_defender
        else:  # attacker communicates slow
            prob *= 1.0 - self.gamma
            deliver = from_defender + from_attacker
        # show blocks to defender, update preference according to proto spec
        for b in deliver:
            e.set_known(b)
            pref = self.protocol.preference(
                e.defender_view(),
                old=e.defender_prefers(),
                new=b,
            )
            e.set_defender_prefers(pref)
        # mine next block
        if alpha:  # attacker mines next block
            prob *= self.alpha
            parents = self.protocol.mining(
                e.attacker_view(),
                e.attacker_prefers(),
            )
            b = e.append(parents, Miner.Attacker)
            if self.force_consider_own:
                e.set_considered(b)
                pref = self.protocol.preference(
                    e.attacker_view(),
                    old=e.attacker_prefers(),
                    new=b,
                )
                e.set_attacker_prefers(pref)
        else:
            prob *= 1 - self.alpha
            parents = self.protocol.mining(
                e.defender_view(),
                e.defender_prefers(),
            )
            e.append(parents, Miner.Defender)
        return self.transition(probability=prob)
