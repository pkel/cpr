from enum import IntEnum
from dataclasses import dataclass
from model import Model, Transition
from protocol import Protocol, View
import numpy
import pynauty

Miner = IntEnum("Miner", ["Attacker", "Defender"], start=0)

AttackerView = IntEnum("AttackerView", ["Ignored", "Considered", "Preferred"], start=0)
DefenderView = IntEnum("DefenderView", ["Unknown", "Known", "Preferred"], start=0)
Withholding = IntEnum("Withholding", ["Foreign", "Withheld", "Released"], start=0)

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
        self._parents = [set()]
        self._children = [set()]
        self.av = [AttackerView.Preferred]
        self.dv = [DefenderView.Preferred]
        if first_miner == Miner.Attacker:
            self.wh = [Withholding.Withheld]
        if first_miner == Miner.Defender:
            self.wh = [Withholding.Foreign]
        self.ht = [0]
        assert isinstance(first_miner, Miner)

        assert self.check()

    def _save(self):
        n = self.n
        buf = numpy.full((n + 4, n), 0, dtype=numpy.uint8)
        for b in range(n):
            for p in self._parents[b]:
                buf[b, p] = 1
            buf[-4, b] = self.av[b]
            buf[-3, b] = self.dv[b]
            buf[-2, b] = self.wh[b]
            buf[-1, b] = self.ht[b]
        # TODO. Boolean adjacency matrix could be compressed with numpy.packbits
        return buf.tobytes() + numpy.uint8(n).tobytes()

    def save(self):
        b = self._save()
        assert self.load(b) is None and self._save() == b
        return b

    def load(self, b):
        buf = numpy.frombuffer(b, dtype=numpy.uint8)
        n = int(buf[-1])
        assert buf.shape == ((n + 4) * n + 1,), buf.shape
        buf = buf[:-1].reshape((n + 4, n))

        self.n = n
        self._parents = [set() for _ in range(n)]
        self._children = [set() for _ in range(n)]
        self.av = []
        self.dv = []
        self.wh = []
        self.ht = []
        for b in range(n):
            for p in range(n):
                if buf[b, p]:
                    self._parents[b].add(p)
                    self._children[p].add(b)
            self.av.append(AttackerView(buf[-4, b]))
            self.dv.append(DefenderView(buf[-3, b]))
            self.wh.append(Withholding(buf[-2, b]))
            self.ht.append(buf[-1, b])

        assert self.check()

    def check(self):
        # shape and lengths
        assert len(self._parents) == self.n
        assert len(self._children) == self.n
        assert len(self.av) == self.n
        assert len(self.dv) == self.n
        assert len(self.wh) == self.n
        assert len(self.ht) == self.n
        # adjacency
        for lst in self._parents + self._children:
            for b in lst:
                assert b >= 0 and b < self.n
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
            isinstance(self.ht[i], int)
        # topological order
        for b in range(self.n):
            for p in self._parents[b]:
                assert p < b, "topological order"
        # height
        for b in range(self.n):
            ph = -1
            for p in self._parents[b]:
                ph = max(ph, self.ht[p])
            assert ph + 1 == self.ht[b], f"height {ph} == {self.ht[b]}"
        # exactly one genesis
        no_parents_cnt = 0
        for b in range(self.n):
            if len(self._parents[b]) == 0:
                no_parents_cnt += 1
        assert no_parents_cnt == 1, f"{no_parents_cnt} root blocks"

        return True

    def parents(self, b):
        return self._parents[b].copy()

    def children(self, b):
        return self._children[b].copy()

    def miner(self, b):
        if self.wh[b] == Withholding.Foreign:
            return Miner.Defender
        else:
            return Miner.Attacker

    def height(self, b):
        return self.ht[b]

    def append(self, parents: set[int], miner: Miner):
        assert isinstance(miner, Miner)
        assert isinstance(parents, set)
        for p in parents:
            assert isinstance(p, int), f"p = {p}"

        # update editor in place
        new = self.n
        self.n += 1
        self._parents.append(parents.copy())
        self._children.append(set())
        for p in parents:
            self._children[p].add(new)
        self.av.append(AttackerView.Ignored)
        self.dv.append(DefenderView.Unknown)
        if miner == Miner.Attacker:
            self.wh.append(Withholding.Withheld)
        elif miner == Miner.Defender:
            self.wh.append(Withholding.Foreign)
        else:
            assert False, "unkown miner"
        self.ht.append(max([self.ht[p] for p in parents]) + 1)
        # safety check
        assert self.check()
        assert isinstance(new, int)
        return new

    def reorder_and_filter(self, old_ids: list[int]):
        assert isinstance(old_ids, list), old_ids
        assert len(old_ids) > 0
        assert len(old_ids) == len(set(old_ids))

        # recall old state
        old_parents = self._parents
        old_children = self._children
        old_av = self.av
        old_dv = self.dv
        old_wh = self.wh
        old_ht = self.ht

        # update in-place
        self.n = len(old_ids)
        self._parents = []
        self._children = []
        self.av = []
        self.dv = []
        self.wh = []
        self.ht = []

        # copy stuff in new order
        new_id = {old: new for new, old in enumerate(old_ids)}

        def f(s):
            return {new_id[x] for x in s if x in new_id}

        for old in old_ids:
            self._parents.append(f(old_parents[old]))
            self._children.append(f(old_children[old]))
            self.av.append(old_av[old])
            self.dv.append(old_dv[old])
            self.wh.append(old_wh[old])
            self.ht.append(old_ht[old])

        # genesis height zero
        delta_height = min(self.ht)
        self.ht = [x - delta_height for x in self.ht]

        # safety check
        assert self.check()

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

    def topologically_ordered(self, blocks: list[int]):
        # Sort the list of blocks topologically, i.e. so that parents are
        # listed before their children.

        assert isinstance(blocks, list)
        for i in blocks:
            assert isinstance(i, int)

        prio = [(self.ht[b], pos, b) for pos, b in enumerate(blocks)]
        return [b for _, _, b in sorted(prio)]

    def canonically_ordered(self, blocks: list[int]):
        # nauty is a C library for finding canonical representations of graphs
        # and checking for isomorphism. pynauty provides Python bindings.
        #
        # This function derives a pynauty graph isomorphic to the edited state
        # or a subset thereof, defined by the given list of blocks. It then
        # calculates canonical labels. The labels are used to return a
        # canonically sorted list of blocks.

        if blocks is None:
            blocks = range(self.blocks)

        assert isinstance(blocks, list)
        assert len(blocks) > 0
        for i in blocks:
            assert isinstance(i, int)
        assert len(blocks) == len(set(blocks))

        n = len(blocks)

        # Nauty works on colored graphs. The state space is already designed
        # to avoid redundant (av, dv, wh) tuples. We translate these tuples
        # to 3 * 3 * 3 = 27 colors. The enums are also integers, so the
        # translation is straight forward.

        # pynauty color representation: list of sets
        colors = [set() for _ in range(27)]  # vertex coloring
        for i in range(n):
            c = (self.dv[i] * 9) + (self.av[i] * 3) + self.wh[i]
            colors[c].add(i)

        # pynauty expects non-empty color sets
        vc = list()
        # palette = list()
        # NOTE: The palette would be required to reconstruct the state from the
        # colored graph. We don't do this though.
        for i in range(27):
            if len(colors[i]) > 0:
                # palette.append(i)
                vc.append(colors[i])

        # pynauty graph representation: adjacency dict
        ad = dict()
        nauty_id = {old: new for new, old in enumerate(blocks)}
        for i, b in enumerate(blocks):
            ad[i] = list()
            for p in self.parents(b):
                if p in blocks:
                    ad[i].append(nauty_id[p])

        # init pynauty Graph
        g = pynauty.Graph(
            n,
            directed=True,
            adjacency_dict=ad,
            vertex_coloring=vc,
        )

        # find canonical labels and return
        canon = pynauty.canon_label(g)
        # NOTE: Is this a list of new labels or a list of old labels?
        # Pynauty doc says: "A list with each node relabelled"
        # Nauty doc says: "The canonical label is given in the form of a list
        # of the vertices of g in canonical order"
        # So, if Pynauty does no fiddle with the result, it should be a list of
        # old labels.

        # map the canonical labels back to block ids
        canon_blocks = [blocks[i] for i in canon]
        # This is a canonically sorted list of blocks

        # To maintain the invariant that block ids are topologically ordered
        # we reorder the topologically.
        return self.topologically_ordered(canon_blocks)


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

    def height(self, b):
        return self.view.height(b)


class SelfishMining(Model):
    def __init__(
        self,
        protocol: Protocol,
        *args,
        alpha: float,
        gamma: float,
        maximum_size: int,
        force_consider_own: bool = True,
        merge_isomorphic: bool = True,
    ):
        assert isinstance(protocol, Protocol)
        assert alpha >= 0 and alpha <= 1
        assert gamma >= 0 and gamma <= 1
        assert maximum_size > 0
        self.protocol = protocol
        self.alpha = alpha
        self.gamma = gamma
        self.maximum_size = maximum_size
        self.force_consider_own = force_consider_own
        self.merge_isomorphic = merge_isomorphic

        self.editor = Editor(first_miner=Miner.Attacker)

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

        # truncate common history, keep only reachable blocks
        keep = set()
        for entrypoint in [e.attacker_prefers(), e.defender_prefers()]:
            keep.add(entrypoint)
            keep |= e.ancestors(entrypoint)
            keep |= e.descendants(entrypoint)
        keep -= e.ancestors(common_ancestor)
        keep = sorted(list(keep))

        # we'll apply keep filter and canonical ordering in one go!
        if self.merge_isomorphic:
            canonkeep = e.canonically_ordered(list(keep))
            e.reorder_and_filter(canonkeep)
        else:
            e.reorder_and_filter(keep)

        return Transition(
            state=self.editor.save(),
            probability=probability,
            progress=progress,
            reward=rew_atk,
        )

    def start(self) -> list[tuple[State, float]]:
        lst = []
        self.editor = Editor(first_miner=Miner.Attacker)
        lst.append((self.editor.save(), self.alpha))
        self.editor = Editor(first_miner=Miner.Defender)
        lst.append((self.editor.save(), 1 - self.alpha))
        return lst

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

    def apply(self, a: Action, s: State) -> list[Transition]:
        if isinstance(a, Release):
            return self.apply_release(a.i, s)
        if isinstance(a, Consider):
            return self.apply_consider(a.i, s)
        if isinstance(a, Continue):
            return self.apply_continue(s)
        assert isinstance(a, Action)
        assert False, "unknown action"

    def apply_release(self, i: int, s: State) -> list[Transition]:
        e = self.editor
        e.load(s)
        # which block will be released?
        b = e.to_release()[i]
        # mark b as released
        e.set_released(b)
        # this transition is deterministic
        lst = [self.transition(probability=1)]
        return lst

    def apply_consider(self, i: int, s: State) -> list[Transition]:
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
        return lst

    def apply_continue(self, s: State) -> list[Transition]:
        lst = []
        for gamma in [True, False]:
            for alpha in [True, False]:
                lst.append(self._apply_continue(s, gamma, alpha))
        return lst

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
