from dataclasses import dataclass
from model import Action, Model, State, StateEditor, Transition, TransitionList
from protocol import Block, Protocol, View


@dataclass
class Config:
    alpha: float
    gamma: float
    protocol: Protocol

    invalid_reward: float = 0.0
    truncate_on_pow: int = -1

    def __post_init__(self):
        if self.alpha < 0 or self.alpha > 1:
            raise ValueError("alpha must be between 0 and 1")
        if self.gamma < 0 or self.gamma > 1:
            raise ValueError("gamma must be between 0 and 1")


class StateEditor(StateEditor):
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

    def clear(self) -> None:
        """
        Remove all blocks from the state.
        """
        raise NotImplementedError


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


def trace(t: Trace, *args, block_mined=False) -> Trace:
    return Trace(
        blocks_mined=t.blocks_mined + block_mined, actions_taken=t.actions_taken + 1
    )


class SelfishMining(Model):
    def __init__(self, editor: StateEditor, config: Config):
        self.editor = editor
        self.config = config
        # Use empty (no blocks) state as sink after taking invalid actions.
        self.editor.clear()
        self.invalid_state = self.editor.save()

    def start(self) -> TransitionList:
        # Any genesis we set here will be rewarded in the future. To make this
        # fair, we set two genesis blocks: one mined by the attacker and one by
        # the defender.
        se = self.editor
        cfg = self.config
        t = Trace(actions_taken=0, blocks_mined=0)
        # prefer attacker
        se.clear()
        b = se.append(set(), mined_by_defender=False)
        se.set_considered_by_attacker(b)
        se.set_released_by_attacker(b)
        se.set_known_to_defender(b)
        se.set_preferred_by_defender(b)
        se.set_preferred_by_attacker(b)
        t1 = Transition(state=se.save(), probability=cfg.alpha, trace=t)
        # prefer defender
        se.clear()
        b = se.append(set(), mined_by_defender=True)
        se.set_considered_by_attacker(b)
        se.set_known_to_defender(b)
        se.set_preferred_by_defender(b)
        se.set_preferred_by_attacker(b)
        t2 = Transition(state=se.save(), probability=1 - cfg.alpha, trace=t)
        # return
        return TransitionList([t1, t2])

    def actions(self, s: State) -> list[Action]:
        se = self.editor
        se.load(s)
        if se.n_blocks == 0:
            return []
        lst = [Continue()]
        for i in se.invert(se.released_by_attacker) - se.mined_by_defender:
            lst.append(Release(i))
        for i in se.invert(se.considered_by_attacker):
            lst.append(Consider(i))
        return lst

    def apply_release(self, b: Block, s: State, t: Trace) -> TransitionList:
        se = self.editor
        se.load(s)
        # safeguard
        assert b not in se.released_by_attacker, f"block {b} already released"
        assert b not in se.mined_by_defender, f"block {b} cannot be released"
        # mark i and all ancestors as released
        for x in {b} | se.ancestors(b) - se.mined_by_defender:
            se.set_released_by_attacker(x)
        # this transition is deterministic
        to = Transition(state=se.save(), probability=1, trace=trace(t))
        return TransitionList([to])

    def apply_consider(self, b: Block, s: State, t: Trace) -> TransitionList:
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
        to = Transition(state=se.save(), probability=1, trace=trace(t))
        return TransitionList([to])

    # apply_continue has four cases which we handle individually here
    def _apply_continue(
        self,
        s: State,
        t: Trace,
        *args,
        attacker_communicates_fast: bool,
        attacker_mines_next_block: bool,
    ) -> Transition:
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
        return Transition(
            state=se.save(), probability=p, trace=trace(t, block_mined=True)
        )

    def apply_continue(self, s: State, t: Trace) -> TransitionList:
        lst = []
        for i in [True, False]:
            for j in [True, False]:
                to = self._apply_continue(
                    s,
                    t,
                    attacker_communicates_fast=i,
                    attacker_mines_next_block=j,
                )
                lst.append(to)
        return TransitionList(lst)

    def apply(self, a: Action, s: State, t: Trace) -> TransitionList:
        # handle termination
        top = self.config.truncate_on_pow
        if top > 0 and t.blocks_mined >= top:
            # we just do a reset here for now
            # TODO. Calculate reward of actually applying the action but
            # transition to terminal state or similar
            return self.start()
        # handle action
        if isinstance(a, Release):
            return self.apply_release(a.b, s, t)
        if isinstance(a, Consider):
            return self.apply_consider(a.b, s, t)
        if isinstance(a, Continue):
            return self.apply_continue(s, t)
        assert False, "invalid action"

    def apply_invalid(self, s: State, t: Trace) -> TransitionList:
        to = Transition(
            state=self.invalid_state,
            probability=1,
            reward=self.config.invalid_reward,
            trace=Trace(blocks_mined=t.blocks_mined, actions_taken=t.actions_taken + 1),
        )
        return TransitionList([to])
