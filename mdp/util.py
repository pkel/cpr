import barzur20aft
import numpy
from time import time


def optimize_and_evaluate(mdp, *args, eps, horizon):
    start = time()

    # map input problem to PTO space
    ptmdp = barzur20aft.ptmdp(mdp, horizon=horizon)

    # find optimal policy
    vi = ptmdp.value_iteration(stop_delta=eps, eps=None, discount=1)

    policy = vi.pop("vi_policy")
    vi_value = vi.pop("vi_value")

    # find promising region in state space
    best_state = numpy.argmax(vi_value)
    vi["vi_max_value"] = vi_value[best_state]

    # find steady state
    ss = mdp.steady_state(policy, start_state=best_state)
    ss_vec = ss.pop("ss")

    # how often do we have to iterate to get at the target precision?
    ptpe = ptmdp.policy_evaluation(
        policy, theta=eps, discount=0, around_state=best_state
    )

    # iterate that often in the (divergent) input mdp
    pe = mdp.policy_evaluation(
        policy, theta=0, discount=0, around_state=best_state, max_iter=ptpe["pe_iter"]
    )

    # calculate PTO revenue (those two should be about the same)
    ptrev = ptpe["pe_reward"][:-1].dot(ss_vec)
    vi_steady_value = vi_value[:-1].dot(ss_vec)

    # calculate reward per progress in source MDP
    steady_rew = pe.pop("pe_reward").dot(ss_vec)
    steady_prg = pe.pop("pe_progress").dot(ss_vec)
    rpp = steady_rew / steady_prg

    res = dict(
        ptrev=ptrev, rpp=rpp, time=time() - start, vi_steady_value=vi_steady_value
    )

    return vi | ss | pe | res
