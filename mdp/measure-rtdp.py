# reference models
import fc16sapirshtein
import aft20barzur

# generic model
from bitcoin import Bitcoin
from sm import SelfishMining

# solving algorithm
from rtdp import RTDP
from compiler import Compiler

# generic tools
import argparse
import joblib
import numpy
import pandas
import pickle
from time import time
import traceback
from tqdm import tqdm


# What do we measure? Table headings ...

columns = [
    dict(alpha=1 / 4, gamma=1 / 4, attacker="weak"),
    dict(alpha=1 / 3, gamma=1 / 3, attacker="intermediate"),
    dict(
        alpha=0.45, gamma=0.90, attacker="strong"
    ),  # TODO double check whether we can do 1/2
]

rows = [
    dict(row=1, protocol="bitcoin", model="fc16", trunc=40, algo="aft20", ref=1),
    dict(row=2, protocol="bitcoin", model="aft20", trunc=40, algo="aft20", ref=1),
    dict(row=3, protocol="bitcoin", model="fc16", trunc=40, algo="rtdp", ref=1),
    dict(row=4, protocol="bitcoin", model="aft20", trunc=40, algo="rtdp", ref=1),
    dict(row=5, protocol="bitcoin", model="fc16", trunc=0, algo="rtdp", ref=1),
    dict(row=6, protocol="bitcoin", model="aft20", trunc=0, algo="rtdp", ref=1),
    #  dict(row=7, protocol="bitcoin", model="generic", trunc=10, algo="aft20", ref=1),
    #  dict(row=8, protocol="bitcoin", model="generic", trunc=10, algo="rtdp", ref=1),
    #  dict(row=9, protocol="bitcoin", model="generic", trunc=0, algo="rtdp", ref=5),
]


# Algorithms


def post_algo(mdp, policy, value_estimate, start_value, start_progress):
    value_estimate = numpy.array(value_estimate)

    # Steady States: I thought it would be cool to report on steady states
    # value and progress. But there is a flaw: in a probabilistically
    # terminating MDPs, the steady state of a policies are the connected
    # terminal states. These do not have rewards. So steady states make no
    # sense, at all!
    # TODO still, as the start state is not necessarily fair (a policy might
    # avoid going back to the start state), start values estimates can be
    # biased.

    # steady state of policy in mdp (via policy-induced markov chain)
    best_state = numpy.argmax(value_estimate)
    ss = mdp.steady_state(policy, start_state=best_state)
    ssvec = ss["ss"]
    assert sum(ssvec) >= 0.9999

    # alternatively, work on policy-induced markov chain
    # get policy-induced markov chain (dict of matrices prb, rew, prg)
    pimc = mdp.markov_chain(policy, start_state=best_state)

    # steady state
    pimc_ss = mdp._steady_state_mc(pimc["prb"])
    pimc_ssvec = pimc_ss["ss"]

    # calculate steady state reward and progress
    pimc_ssvec  # prob to be in a state
    pimc[
        "prb"
    ]  # one row per source state, probability of transitioning to target state
    pimc["rew"]  # one row per source state, reward when transitioning to target state
    pimc["prg"]  # one row per source state, progress when transitioning to target state
    pimc_erew = (
        (pimc["prb"] * pimc["rew"]).sum(axis=1).A1
    )  # expected reward of next step
    pimc_eprg = (
        (pimc["prb"] * pimc["prg"]).sum(axis=1).A1
    )  # expected progress of next step
    pimc_ss_rew = pimc_erew.dot(pimc_ssvec)  # steady state weighted next reward
    pimc_ss_prg = pimc_eprg.dot(pimc_ssvec)  # steady state weighted next progress

    # print(pimc['prb'].sum(axis = 1).A1) # sum of ones? YES

    return dict(
        start_value=start_value,
        start_progress=start_progress,
        mdp_n_states=mdp.n_states,
        mdp_n_transitions=mdp.n_transitions,
        ss_n_states_reachable=ss["ss_reachable"],
        ss_n_states_nonzero=ss["ss_nonzero"],
        ss_value=value_estimate.dot(ssvec),  # why are these negative an close to zero??
        ss_time=ss["ss_time"],
        pimc_n_states=pimc["prb"].get_shape()[0],
        pimc_ss_reward=pimc_ss_rew,
        pimc_ss_progress=pimc_ss_prg,
    )


def algo_aft20(implicit_mdp, *args, horizon, vi_delta, **kwargs):
    # Compile Full MDP
    mdp = Compiler(implicit_mdp).mdp()

    # Derive PTO MDP
    mdp = aft20barzur.ptmdp(mdp, horizon=horizon)

    # Solve PTO MDP
    vi = mdp.value_iteration(stop_delta=vi_delta, eps=None, discount=1)
    policy = vi["vi_policy"]

    start_value = 0.0
    start_progress = 0.0
    for state, prob in mdp.start.items():
        start_value += vi["vi_value"][state] * prob
        start_progress += vi["vi_progress"][state] * prob

    return post_algo(mdp, policy, vi["vi_value"], start_value, start_progress)


def algo_rtdp(implicit_mdp, *args, horizon, rtdp_steps, rtdp_eps, **kwargs):
    agent = RTDP(implicit_mdp, eps=rtdp_eps, eps_honest=0, horizon=horizon)

    for i in range(rtdp_steps):
        agent.step()

    m = agent.mdp()
    start_value, start_progress = agent.start_value_and_progress()

    return post_algo(m["mdp"], m["policy"], m["value"], start_value, start_progress)


# How do we instantiate the models and run the algo?


def implicit_mdp(*args, model, protocol, trunc, alpha, gamma, **kwargs):
    if model in ["fc16", "aft20"]:
        assert protocol == "bitcoin", "fc16 and aft20 model are bitcoin-only"

    common = dict(alpha=alpha, gamma=gamma)

    if trunc <= 0:
        trunc = 100_000
    # TODO disable truncation completely

    if model == "fc16":
        return fc16sapirshtein.BitcoinSM(**common, maximum_fork_length=trunc)

    if model == "aft20":
        return aft20barzur.BitcoinSM(**common, maximum_fork_length=trunc)

    if model == "generic":
        common["merge_isomorphic"] = False
        common["maximum_size"] = trunc

        if protocol == "bitcoin":
            return SelfishMining(Bitcoin(), **common)

        raise ValueError(f"unknown protocol: {protocol}")

    raise ValueError(f"unknown model: {model}")


# Command line arguments

argp = argparse.ArgumentParser()
argp.add_argument("-j", "--n_jobs", type=int, default=1, metavar="INT")
argp.add_argument("-H", "--horizon", type=int, default=30, metavar="INT")
argp.add_argument("--rtdp_eps", type=float, default=0.25, metavar="FLOAT")
argp.add_argument("--rtdp_steps", type=int, default=50_000, metavar="INT")
argp.add_argument("--vi_delta", type=float, default=0.01, metavar="FLOAT")
args = argp.parse_args()

# Single measurement


def measure_unsafe(*_args, algo, **kwargs):
    mdp = implicit_mdp(**kwargs)
    kwargs["horizon"] = args.horizon
    if algo == "aft20":
        hp = dict(vi_delta=args.vi_delta)
        return algo_aft20(mdp, **hp, **kwargs) | dict(hyperparams=hp)
    if algo == "rtdp":
        hp = dict(rtdp_eps=args.rtdp_eps, rtdp_steps=args.rtdp_steps)
        return algo_rtdp(mdp, **hp, **kwargs) | dict(hyperparams=hp)

    raise ValueError(f"unknown algo: {algo}")


def measure(*args, **kwargs):
    try:
        return measure_unsafe(*args, **kwargs)
    except Exception as e:
        return dict(error=str(e), traceback=traceback.format_exc())


# Multicore measurement loop


def job(*args, **kwargs):
    start_time = time()
    return kwargs | measure(**kwargs) | dict(time=time() - start_time)


def job_gen():
    for r in rows:
        for c in columns:
            yield joblib.delayed(job)(**r, **c)


jobs = list(job_gen())

res_gen = joblib.Parallel(n_jobs=args.n_jobs, return_as="generator")(jobs)

print()
print(f"Run {len(jobs)} jobs on {args.n_jobs} threads ...")

rows = []
for res in tqdm(res_gen, total=len(jobs)):
    rows.append(res)

    if "error" in res:
        print(res["traceback"])

df = pandas.DataFrame(rows)

# Save to disk

fname = "measure-rtdp.pkl"
print()
print(f"storing results in {fname}")

results = dict(
    data=df,
)

with open(fname, "wb") as pkl:
    pickle.dump(results, pkl)

# Print

print(df.drop(columns=["hyperparams"]))

# Error handling

if "error" in df.columns:
    for idx, r in df[~df.error.isna()].iterrows():
        print(r)
        print(r.traceback)
        print()

    raise Exception("errors during measurements")
