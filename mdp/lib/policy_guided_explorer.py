# implementing the above loop as incremental exploration

from copy import deepcopy
from . import explicit_mdp as emdp


# Invariant:
# - policy-defined actions are explored first
# - in the result MDP: policy = lambda s: 0
# - states are enumerated in order of exploration
# - states far off the policy have higher state ids than states closer or on the policy
# - policies for smaller MDPs are compatible with policies for bigger MDPs
class Explorer:
    def __init__(self, model, policy):
        self.model = model
        self.policy = policy

        self._mdp = emdp.MDP()
        self.states = []  # state id int -> state
        self.policy_tab = []  # state id int -> action

        self._state_id = dict()  # state -> state id int; see self.state_id()
        self.explored_upto = -1  # state id int; policy action has been explored
        self.fully_explored_upto = -1  # state id int; all actions have been explored

        for s, p in self.model.start():
            self._mdp.start[self.state_id(s)] = p

    def state_id(self, state):
        if state in self._state_id:
            return self._state_id[state]
        else:
            i = len(self._state_id)
            self._state_id[state] = i
            self.states.append(state)
            return i

    @property
    def n_states(self):
        return len(self._state_id)

    @property
    def max_state_id(self):
        return len(self._state_id) - 1

    def explore_along_policy(self, max_states: int = -1):
        while self.max_state_id > self.explored_upto:
            # optional logic to abort exploration beyond a certain size
            if max_states > 0 and self.n_states > max_states:
                raise RuntimeError("state size limit exceeded")

            # ---

            self.explored_upto += 1
            s_id = self.explored_upto
            s = self.states[s_id]

            assert (
                len(self.policy_tab) == s_id
            ), f"logic error, {len(self.policy_tab)} == {s_id}"

            if len(self.model.actions(s)) == 0:
                # s is a terminal state
                self.policy_tab.append(-1)  # policy is not defined, put placeholder
                continue

            a = self.policy(s)
            self.policy_tab.append(a)

            for t in self.model.apply(a, s):
                if t.probability == 0:
                    continue

                t = emdp.Transition(
                    probability=t.probability,
                    destination=self.state_id(t.state),
                    reward=t.reward,
                    progress=t.progress,
                    effect=t.effect,
                )
                self._mdp.add_transition(src=s_id, act=0, t=t)

            self.explored_upto = s_id

    def explore_aside_policy(self, *, max_states: int = -1):
        self.explore_along_policy()
        assert self.explored_upto == self.max_state_id

        while self.fully_explored_upto < self.explored_upto:
            # optional logic to abort exploration beyond a certain size
            if max_states > 0 and self.n_states > max_states:
                raise RuntimeError("state size limit exceeded")

            # ---

            self.fully_explored_upto += 1
            s_id = self.fully_explored_upto
            s = self.states[s_id]

            a_idx = 0  # policy action has a_idx = 0
            for a in self.model.actions(s):
                if a == self.policy_tab[s_id]:
                    # policy action; already explored
                    continue

                a_idx += 1

                for t in self.model.apply(a, s):
                    if t.probability == 0:
                        continue

                    t = emdp.Transition(
                        probability=t.probability,
                        destination=self.state_id(t.state),
                        reward=t.reward,
                        progress=t.progress,
                        effect=t.effect,
                    )
                    self._mdp.add_transition(src=s_id, act=a_idx, t=t)

    def mdp(self, **kwargs):
        # Note 1. For some states in self._mdp; we've only explored the honest action.
        # That's okay, it forces the attacker to abort the attack.
        # Note 2. Some states are reachable but not yet explored; we have to fix this before
        # returning the MDP.
        self.explore_along_policy(**kwargs)
        assert self.explored_upto == self.max_state_id

        self._mdp.check()

        return deepcopy(self._mdp)
