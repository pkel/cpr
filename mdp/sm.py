from enum import IntEnum
from dataclasses import dataclass, replace
from mdp import MDP
import pickle
from model import Effect, Model, Transition
from protocol import Protocol, View
import pynauty
import subprocess

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


@dataclass(frozen=True)
class Communicate(Action):
    pass


class Editor(View):
    def __init__(self, *args, first_miner: Miner):
        self.n = 1
        self._parents = [set()]
        self._children = [set()]
        self.av = [AttackerView.Preferred]
        self.dv = [DefenderView.Preferred]
        if first_miner == Miner.Attacker:
            self.wh = [Withholding.Released]
        if first_miner == Miner.Defender:
            self.wh = [Withholding.Foreign]
        self.ht = [0]
        assert isinstance(first_miner, Miner)

        assert self.check()

    def _save(self):
        data = (
            self.n,
            self._parents,
            self._children,
            self.av,
            self.dv,
            self.wh,
            self.ht,
        )
        return pickle.dumps(data)

    def save(self):
        b = self._save()
        assert self.load(b) is None and self._save() == b
        assert isinstance(b, State)
        return b

    def load(self, b):
        data = pickle.loads(b)
        self.n, self._parents, self._children, self.av, self.dv, self.wh, self.ht = data
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
        assert all(x >= 0 for x in self.ht)
        for b in range(self.n):
            ph = -1  # parent height
            for p in self._parents[b]:
                ph = max(ph, self.ht[p])
            assert ph + 1 == self.ht[b], f"height {ph + 1} == {self.ht[b]}"
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

    def reinit_height(self):
        assert False, "don't think this is used somewhere"
        roots = {i for i in range(self.n) if len(self.parents(i)) == 0}
        h = 0
        level = roots
        while len(level) > 0:
            next_level = set()
            for i in level:
                next_level |= self.children(i)
                self.ht[i] = h
            h += 1
            level = next_level

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

        def all_parents_or_none():
            for b in old_ids:
                removed = [p in old_ids for p in self.parents(b)]
                assert all(removed) or not any(removed)
            return True

        assert all_parents_or_none()
        # anything else could break self.ht

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

        # adjust height such that genesis has height zero
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
        # For me this hints at a list of new labels:
        # canon[<id in g>] = <canon id>
        # Nauty doc says: "The canonical label is given in the form of a list
        # of the vertices of g in canonical order". So, if Pynauty does not
        # fiddle with the result, it should be a list of old labels:
        # canon[<canon id>] = <id in g>

        # map the canonical labels back to block ids
        canon_blocks = [blocks[g_id] for canon_id, g_id in enumerate(canon)]

        # Alternative interpretation:
        # canon_blocks = [blocks[canon_id] for canon_id, g_id in enumerate(canon)]
        # When I use it, less states are merged. Results look the same expect
        # that the models grow bigger. So it seems that the above
        # interpretation is right!

        # Anyhow, it is important to note that we only use these canonical
        # labels to reindex the blocks in our state. By reordering alone we
        # cannot invalidate the state transitions. Merging two states that
        # should not be merged is not impossible.

        # canon_blocks is a canonically sorted list of blocks.
        # To maintain the invariant that block ids are topologically ordered
        # we reorder them topologically.
        return self.topologically_ordered(canon_blocks)

    def graph_easy(self, info=dict()):
        lbl = []
        lns = []
        for b in range(self.n):
            av = self.av[b].name[0]
            dv = self.dv[b].name[0]
            wh = self.wh[b].name[0]
            ht = self.ht[b]
            xx = ""
            if b in info:
                xx = " " + str(info[b])
            lbl.append(f"{b}: {av}/{dv}/{wh}/{ht}{xx}")
            lns.append(f"[{lbl[-1]}]")
        for b in range(self.n):
            for p in self.parents(b):
                lns.append(f"[{lbl[b]}] --> [{lbl[p]}]")
        return "\n".join(lns)

    def asciify(self, info=dict()):
        rendered = subprocess.run(
            ["graph-easy"], input=self.graph_easy(info), text=True, capture_output=True
        )
        rendered.check_returncode()
        return rendered.stdout

    def debug_print(self, info=dict()):
        print(self.asciify(info))

    def __repr__(self):
        return self.asciify()


class PartialView(View):
    def __init__(self, view: View, filter=lambda _: True):
        # check argument; one block should be visible
        assert any(filter(i) for i in range(view.n))

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
        maximum_height: int = 0,
        maximum_size: int = 0,
        force_consider_own: bool = True,
        merge_isomorphic: bool = True,
    ):
        assert isinstance(protocol, Protocol)
        assert alpha >= 0 and alpha <= 1
        assert gamma >= 0 and gamma <= 1
        assert maximum_height > 0 or maximum_size > 0, "infinite state space"
        assert maximum_size > 0 or force_consider_own, "infinite state space"
        self.protocol = protocol
        self.alpha = alpha
        self.gamma = gamma
        self.maximum_height = maximum_height
        self.maximum_size = maximum_size
        self.force_consider_own = force_consider_own
        self.merge_isomorphic = merge_isomorphic

        self.editor = Editor(first_miner=Miner.Attacker)

    def __repr__(self):
        return (
            f"sm.SelfishMining({self.protocol}, "
            f"alpha={self.alpha}, "
            f"gamma={self.gamma}, "
            f"maximum_height={self.maximum_height}, "
            f"maximum_size={self.maximum_size}, "
            f"force_consider_own={self.force_consider_own})"
        )

    def history(self, block):
        e = self.editor
        hist = []
        while block is not None:
            hist.insert(0, block)
            block = self.protocol.predecessor(e, block)
        return hist

    def common_history(self, hist_a, hist_b):
        assert hist_a[0] == hist_b[0], "old ca"

        i = 0
        common = []
        max_i = min(len(hist_a), len(hist_b))
        while i < max_i:
            x = hist_a[i]
            if x == hist_b[i]:
                common.append(x)
            else:
                break
            i += 1
        return common

    def transition(self, *args, probability, defender_preferred_before, block_mined):
        e = self.editor

        atk_pref = e.attacker_prefers()
        def_pref = e.defender_prefers()

        # histories
        atk_hist = self.history(atk_pref)
        def_hist = self.history(def_pref)
        def_hist_old = self.history(defender_preferred_before)

        # measure rewriting (defender old vs new)
        assert len(def_hist) >= len(def_hist_old)
        unchanged_history = self.common_history(def_hist_old, def_hist)
        assert len(unchanged_history) > 0, "genesis cannot be rewritten"
        rewrite_length = len(def_hist_old) - len(unchanged_history)
        assert rewrite_length >= 0

        rewrite_prg_beg = self.protocol.progress(e, unchanged_history[-1])
        rewrite_prg_end = self.protocol.progress(e, defender_preferred_before)
        rewrite_prg = rewrite_prg_end - rewrite_prg_beg
        assert rewrite_prg >= 0.0
        assert rewrite_prg == 0.0 or rewrite_length > 0.0

        def_prg_was = rewrite_prg_end
        def_prg_now = self.protocol.progress(e, def_pref)
        def_prg_delta = def_prg_now - def_prg_was
        assert def_prg_delta >= 0.0

        # find common history (attacker vs defender)
        common_history = self.common_history(atk_hist, def_hist)
        assert len(common_history) > 0, "genesis should be agreed upon"

        common_ancestor = common_history[-1]

        # define which blocks to keep: only reachable blocks
        reachable = set()
        for entrypoint in [atk_pref, def_pref]:
            reachable.add(entrypoint)
            reachable |= e.descendants(entrypoint)
            for d in reachable.copy():
                reachable |= e.ancestors(d)

        truncate = e.ancestors(common_ancestor)
        keep = reachable - truncate

        # we cannot remove some parents; either all or none
        def should_also_keep(keep):
            sak = set()
            for b in keep:
                parents = e.parents(b)
                removed = [p in keep for p in parents]
                if all(removed) or not any(removed):
                    continue
                else:
                    sak |= parents
            return sak - keep

        # we cannot have two roots
        def roots(keep):
            acc = set()
            for b in keep:
                parents = e.parents(b) & keep
                if len(parents) == 0:
                    acc.add(b)
            return acc

        while len(roots(keep)) > 1 or len(should_also_keep(keep)) > 0:
            # TODO maybe we can skip ahead instead of shrinking common_history
            # one by one?
            common_history.pop(-1)
            common_ancestor = common_history[-1]
            truncate = e.ancestors(common_ancestor)
            keep = reachable - truncate

        assert len(roots(keep)) == 1

        # calculate rewards
        rew_atk = 0.0
        rew_def = 0.0

        # enforce assumption that rewards do not depend on past history
        def reward_view(x):
            rb = e.ancestors(x) | {x}  # subset of relevant blocks
            next_x = self.protocol.predecessor(e, x)
            if next_x is not None:
                rb -= {next_x} | e.ancestors(next_x)

            return PartialView(e, lambda x: x in rb)

        # common_history[0] == old_ca == 0
        assert common_history[0] == 0
        for x in common_history[1:]:
            for r in self.protocol.reward(reward_view(x), x):
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

        assert (rew_atk == 0 and rew_def == 0) or progress > 0

        # apply the keep filter and (optionally) make canonical
        if self.merge_isomorphic:
            canonkeep = e.canonically_ordered(list(keep))
            e.reorder_and_filter(canonkeep)
        else:
            e.reorder_and_filter(sorted(list(keep)))

        effect = Effect(
            blocks_mined=1.0 if block_mined else 0.0,
            common_atk_reward=rew_atk,
            common_def_reward=rew_def,
            common_progress=progress,
            defender_rewrite_length=rewrite_length,
            defender_rewrite_progress=rewrite_prg,
            defender_progress=def_prg_delta,
        )

        return Transition(
            state=e.save(),
            probability=probability,
            effect=effect,
            # Default to long-term revenue MDP:
            reward=effect.common_atk_reward,
            progress=effect.defender_progress,
        )
        # Progress must be taken from defender chain, as common chain progress
        # might be zero for some policies, e.g. p(s) = Continue(). Zero
        # progress policies significantly slow down exploration.
        # TODO Now that we take progress from the defender's chain, we should
        # also calculate rewards on the defender chain. I did this already in
        # the rust codebase. See commit e68a4f00. This requires a different
        # episode shutdown mechanism, where the agent is forced to release all
        # blocks. See commit 6a3b5a4.

    def acc_effect(self, a, b):
        # It is not clear how to handle defender_* here, depends on the attack
        # scenario, I guess!
        return Effect(
            blocks_mined=a.blocks_mined + b.blocks_mined,
            common_atk_reward=a.common_atk_reward + b.common_atk_reward,
            common_def_reward=a.common_def_reward + b.common_def_reward,
            common_progress=a.common_progress + b.common_progress,
            defender_rewrite_length=max(
                a.defender_rewrite_length, b.defender_rewrite_length
            ),
            defender_rewrite_progress=max(
                a.defender_rewrite_progress, b.defender_rewrite_progress
            ),
            defender_progress=a.defender_progress + b.defender_progress,
        )

    def start(self) -> list[tuple[State, float]]:
        lst = []
        self.editor = Editor(first_miner=Miner.Attacker)
        lst.append((self.editor.save(), self.alpha))
        self.editor = Editor(first_miner=Miner.Defender)
        lst.append((self.editor.save(), 1 - self.alpha))
        return lst

    def actions(self, s: State) -> list[Action]:
        e = self.editor
        e.load(s)

        if self.truncate_now(e):
            # we forbid mining and allow communication only if there is
            # something to communicate. This forces the attacker to consider
            # and release all blocks before reaching a terminal state.
            if len(e.just_released()) + len(e.just_mined_by_defender()) > 0:
                actions = [Communicate()]
            else:
                actions = []

        else:
            # continue is generally feasible
            actions = [Continue()]

        # release/consider when it makes sense
        for i, _ in enumerate(e.to_release()):
            actions.append(Release(i))
        for i, _ in enumerate(e.to_consider()):
            actions.append(Consider(i))

        return actions

    def truncate_now(self, e: Editor) -> bool:
        ms = self.maximum_size
        if ms > 0 and e.n >= ms:
            return True

        mh = self.maximum_height
        if mh > 0 and max(e.ht) >= mh:
            return True

        return False

    def honest(self, s: State) -> Action:
        e = self.editor
        e.load(s)

        # honest policy: release then consider then continue
        if len(e.to_release()) > 0:
            return Release(0)
        if len(e.to_consider()) > 0:
            return Consider(0)

        if self.truncate_now(e):
            return Communicate()
        else:
            return Continue()

    def apply(self, a: Action, s: State) -> list[Transition]:
        if isinstance(a, Release):
            return self.apply_release(a.i, s)
        if isinstance(a, Consider):
            return self.apply_consider(a.i, s)
        if isinstance(a, Continue):
            return self.apply_continue(s)
        if isinstance(a, Communicate):
            return self.apply_communicate(s)
        assert isinstance(a, Action)
        assert False, "unknown action"

    def apply_release(self, i: int, s: State) -> list[Transition]:
        e = self.editor
        e.load(s)
        dpb = e.defender_prefers()
        # which block will be released?
        b = e.to_release()[i]
        # mark b as released
        e.set_released(b)
        # this transition is deterministic
        lst = [
            self.transition(
                probability=1, defender_preferred_before=dpb, block_mined=False
            )
        ]
        return lst

    def apply_consider(self, i: int, s: State) -> list[Transition]:
        e = self.editor
        e.load(s)
        dpb = e.defender_prefers()
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
        lst = [
            self.transition(
                probability=1, defender_preferred_before=dpb, block_mined=False
            )
        ]
        return lst

    def apply_continue(self, s: State) -> list[Transition]:
        lst = []
        for gamma in [True, False]:
            for alpha in [True, False]:
                lst.append(
                    self._apply_continue(
                        s, gamma=gamma, alpha=alpha, communication_only=False
                    )
                )
        return lst

    def apply_communicate(self, s: State) -> list[Transition]:
        lst = []
        for gamma in [True, False]:
            lst.append(
                self._apply_continue(
                    s, gamma=gamma, alpha=None, communication_only=True
                )
            )
        return lst

    def _apply_continue(
        self, s: State, *args, gamma: bool, alpha: bool, communication_only: bool
    ):
        e = self.editor
        e.load(s)
        dpb = e.defender_prefers()
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
        if not communication_only:
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
        return self.transition(
            probability=prob,
            defender_preferred_before=dpb,
            block_mined=not communication_only,
        )

    def shutdown(self, s: State) -> list[Transition]:
        e = self.editor
        e.load(s)

        # Release all blocks
        for b in range(e.n):
            if e.wh[b] == Withholding.Withheld:
                e.wh[b] = Withholding.Released
        s = e.save()

        # Communicate & return
        return self.apply_communicate(s)


mappable_params = dict(alpha=0.125, gamma=0.25)


def map_params(m: MDP, *args, alpha: float, gamma: float):
    assert alpha >= 0 and alpha <= 1
    assert gamma >= 0 and gamma <= 1

    a = mappable_params["alpha"]
    g = mappable_params["gamma"]
    mapping = dict()
    mapping[1] = 1
    mapping[a] = alpha
    mapping[1 - a] = 1 - alpha
    mapping[g] = gamma
    mapping[1 - g] = 1 - gamma
    mapping[a * g] = alpha * gamma
    mapping[(1 - a) * g] = (1 - alpha) * gamma
    mapping[a * (1 - g)] = alpha * (1 - gamma)
    mapping[(1 - a) * (1 - g)] = (1 - alpha) * (1 - gamma)

    assert len(set(mapping.keys())) == 9, "mappable_params are not mappable"

    # map probabilities
    tab = []
    for actions in m.tab:
        new_actions = dict()
        for act, transitions in actions.items():
            new_transitions = []
            for t in transitions:
                new_t = replace(t, probability=mapping[t.probability])
                new_transitions.append(new_t)
            new_actions[act] = new_transitions
        tab.append(new_actions)

    start = dict()
    for state, prob in m.start.items():
        start[state] = mapping[prob]

    new = replace(m, start=start, tab=tab)

    assert new.check()
    return new


# Example (1)
# common ancestor: 5
# to be truncated: right of block 5, marked ✗
#
# problem 1: 4 will lose one of it's parents, which is usually not possible in
# a blockchain.
#
# problem 2: 1 will become a new root, which might confuse parts of my code
#
# problem 3: we certainly have to keep around 4, it might end up getting an
# uncle reward. 1 won't get a reward, but that's protocol related. The SM model
# cannot know.
#
# solution approach: first, we limit the attacker/defender view to descendants
# of the preferred block. 4 and 1 will not be visible, hence fiddling with
# their ancestors will cause no (hopefully) problems. Second, we update the
# height of all (affected) blocks. Here, 4 and 1 will get height 0 and 1.
# Keeping the blocks around will allow signing rewards.
# Third, we cross fingers that multiple roots do not break things. We still
# have the check for exactly one common ancestor, so we should be fine.
#
# This highlights that the reward function can only consider the block given as
# argument and these ancestors which are offside the further history. With the
# current API it is in principle possible that 7 hands out rewards for 3 or 1.
# When we truncate 3 or 1 (we actually do) then these rewards would get lost.
# I'll add a safeguard to the reward function that restrict what can be
# reached.
#
# Amendment. Limiting attacker/defender view does not work. It breaks bitcoin.
# Forcing truncation and recomputing height does not work either. It causes
# permanent chain splits. So, since I already spent too much time on this, I'll
# just not truncate in the problematic cases.
#
#                    +------------+
#                    | 8: C/U/W/5 |
#                    +------------+
#                      |
#                      |
#                      v
# +------------+     +------------+     +------------+     +--------------+     +--------------+     +--------------+ # noqa: E501
# | 7: I/K/F/5 | --> | 6: C/K/R/4 | --> | 5: P/P/R/3 | --> | 3: C/K/R/2 ✗ | --> | 2: C/K/R/1 ✗ | --> | 0: C/K/R/0 ✗ | # noqa: E501
# +------------+     +------------+     +------------+     +--------------+     +--------------+     +--------------+ # noqa: E501
#   |                                                        ^                                         ^              # noqa: E501
#   |                                                        |                                         |              # noqa: E501
#   v                                                        |                                         |              # noqa: E501
# +------------+                                             |                                         |              # noqa: E501
# | 4: I/K/F/3 | --------------------------------------------+                                         |              # noqa: E501
# +------------+                                                                                       |              # noqa: E501
#   |                                                                                                  |              # noqa: E501
#   |                                                                                                  |              # noqa: E501
#   v                                                                                                  |              # noqa: E501
# +------------+                                                                                       |              # noqa: E501
# | 1: I/K/F/1 | --------------------------------------------------------------------------------------+              # noqa: E501
# +------------+
