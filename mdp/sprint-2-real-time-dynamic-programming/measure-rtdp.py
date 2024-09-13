# reference models
from lib.models import fc16sapirshtein
from lib.models import aft20barzur

# generic model
from lib.models.generic_v0.bitcoin import Bitcoin
from lib.models.generic_v0.model import SelfishMining

# solving algorithm
from lib.implicit_mdp import PTO_wrapper
from lib.rtdp import RTDP
from lib.compiler import Compiler

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
    dict(alpha=1 / 4, gamma=1 / 4, attacker="a25"),
    dict(alpha=1 / 3, gamma=1 / 3, attacker="a33"),
    dict(alpha=0.42, gamma=0.84, attacker="a42"),
    dict(alpha=0.45, gamma=0.90, attacker="a45"),
]  # TODO double check whether we can do 1/2

rows = [
    dict(row=1, protocol="bitcoin", model="fc16", trunc=40, algo="aft20", ref=1),
    #  dict(row=2, protocol="bitcoin", model="aft20", trunc=40, algo="aft20", ref=1),
    dict(row=3, protocol="bitcoin", model="fc16", trunc=40, algo="rtdp/mdp", ref=1),
    dict(row=3, protocol="bitcoin", model="fc16", trunc=40, algo="rtdp", ref=1),
    #  dict(row=4, protocol="bitcoin", model="aft20", trunc=40, algo="rtdp", ref=1),
    #  dict(row=5, protocol="bitcoin", model="fc16", trunc=0, algo="rtdp", ref=1),
    #  dict(row=6, protocol="bitcoin", model="aft20", trunc=0, algo="rtdp", ref=1),
    #  dict(row=7, protocol="bitcoin", model="generic", trunc=10, algo="aft20", ref=1),
    #  dict(row=8, protocol="bitcoin", model="generic", trunc=10, algo="rtdp", ref=1),
    #  dict(row=9, protocol="bitcoin", model="generic", trunc=0, algo="rtdp", ref=5),
]


# Algorithms


def post_algo(mdp, policy, value_estimate, pe_theta, **kwargs):
    value_estimate = numpy.array(value_estimate)

    # Steady States: I thought it would be cool to report on steady states
    # value and progress. But there is a flaw: in probabilistically terminating
    # MDPs, the steady state of a policy are the connected terminal states.
    # These do not have rewards. So steady states make no sense, at all!
    # NOTE as the start state is not necessarily fair (a policy might avoid
    # going back to the start state), start value estimates can be biased.
    # NOTE if the horizon is high, this bias should go towards zero?! So let's
    # stop worrying about that for now.

    # alternatively, work on policy-induced markov chain
    # get policy-induced markov chain (dict of matrices prb, rew, prg)
    pimc = mdp.markov_chain(policy, start_state=0)

    best_state = numpy.argmax(value_estimate)
    pe = mdp.policy_evaluation(policy, theta=pe_theta, around_state=best_state)

    pe_start_reward = 0.0
    pe_start_progress = 0.0
    for st, prob in mdp.start.items():
        pe_start_reward += prob * pe["pe_reward"][st]
        pe_start_progress += prob * pe["pe_progress"][st]

    return dict(
        mdp_n_states=mdp.n_states,
        mdp_n_transitions=mdp.n_transitions,
        pimc_n_states=pimc["prb"].get_shape()[0],
        pe_start_reward=pe_start_reward,
        pe_start_progress=pe_start_progress,
        **kwargs,
    )


def algo_aft20(implicit_mdp, *args, horizon, vi_delta, **kwargs):
    implicit_ptmdp = PTO_wrapper(
        implicit_mdp, horizon=horizon, terminal_state=b"terminal"
    )

    # Compile Full MDP
    mdp = Compiler(implicit_ptmdp).mdp()

    # Solve PTO MDP
    vi = mdp.value_iteration(stop_delta=vi_delta, eps=None, discount=1)
    policy = vi["vi_policy"]

    start = dict()
    start["agent_start_reward"] = 0.0
    start["agent_start_progress"] = 0.0
    for state, prob in mdp.start.items():
        start["agent_start_reward"] += vi["vi_value"][state] * prob
        start["agent_start_progress"] += vi["vi_progress"][state] * prob

    return post_algo(mdp, policy, vi["vi_value"], **start, **kwargs)


def algo_rtdp(
    implicit_mdp,
    *args,
    pe_on_mdp=False,
    horizon,
    rtdp_steps,
    rtdp_eps,
    rtdp_es,
    **kwargs,
):
    terminal = b"terminal"
    implicit_ptmdp = PTO_wrapper(implicit_mdp, horizon=horizon, terminal_state=terminal)

    agent = RTDP(implicit_ptmdp, eps=rtdp_eps, eps_honest=0, es=rtdp_es)

    log = []

    i = 0
    j = 0
    log_interval = rtdp_steps / 100
    while i < rtdp_steps:
        i += 1
        agent.step()

        # logging
        j += 1
        if j >= log_interval:
            j = 0
            sv, sp = agent.start_value_and_progress()
            log.append(
                dict(
                    step=i,
                    start_reward=sv,
                    start_progress=sp,
                    n_states=len(agent.states),
                )
            )

    start_reward, start_progress = agent.start_value_and_progress()
    start = dict()
    start["agent_start_reward"] = start_reward
    start["agent_start_progress"] = start_progress

    if pe_on_mdp:
        # prepare eval on full MDP
        compiler = Compiler(implicit_ptmdp)
        mdp = compiler.mdp()
        state_id = compiler.state_map
        policy = agent.policy(state_id=state_id, terminal_state=terminal)
        value = agent.value(state_id=state_id, terminal_state=terminal)
    else:
        rtdp = agent.mdp()
        mdp = rtdp["mdp"]
        policy = rtdp["policy"]
        value = rtdp["value"]

    return post_algo(
        mdp,
        policy,
        value,
        log=log,
        **start,
        **kwargs,
    )


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
argp.add_argument("--rtdp_eps", type=float, default=0.2, metavar="FLOAT")
argp.add_argument("--rtdp_es", type=float, default=0.9, metavar="FLOAT")
argp.add_argument("--rtdp_steps", type=int, default=50_000, metavar="INT")
argp.add_argument("--vi_delta", type=float, default=0.01, metavar="FLOAT")
argp.add_argument("--pe_theta", type=float, default=0.01, metavar="FLOAT")
args = argp.parse_args()

# Single measurement


def measure_unsafe(*_args, algo, **kwargs):
    mdp = implicit_mdp(**kwargs)
    kwargs["horizon"] = args.horizon
    kwargs["pe_theta"] = args.pe_theta
    if algo == "aft20":
        hp = dict(vi_delta=args.vi_delta)
        return algo_aft20(mdp, **hp, **kwargs) | dict(hyperparams=hp)
    if algo == "rtdp":
        hp = dict(
            rtdp_eps=args.rtdp_eps, rtdp_es=args.rtdp_es, rtdp_steps=args.rtdp_steps
        )
        return algo_rtdp(mdp, **hp, **kwargs) | dict(hyperparams=hp)
    if algo == "rtdp/mdp":
        hp = dict(
            rtdp_eps=args.rtdp_eps, rtdp_es=args.rtdp_es, rtdp_steps=args.rtdp_steps
        )
        return algo_rtdp(mdp, **hp, **kwargs, pe_on_mdp=True) | dict(hyperparams=hp)

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
