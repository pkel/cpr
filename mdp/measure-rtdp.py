# reference models
import fc16sapirshtein
import aft20barzur

# generic model
from bitcoin import Bitcoin
from sm import SelfishMining

# solving algorithm
from rtdp import RTDP
from compiler import Compiler
import util

# generic tools
import argparse
import joblib
import pandas
import random
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
    dict(row=1, protocol="bitcoin", model="fc16", truncated=True, algo="aft20", ref=1),
    dict(row=2, protocol="bitcoin", model="aft20", truncated=True, algo="aft20", ref=1),
    dict(row=3, protocol="bitcoin", model="fc16", truncated=True, algo="rtdp", ref=1),
    dict(row=4, protocol="bitcoin", model="aft20", truncated=True, algo="rtdp", ref=1),
    dict(row=5, protocol="bitcoin", model="fc16", truncated=False, algo="rtdp", ref=1),
    dict(row=6, protocol="bitcoin", model="aft20", truncated=False, algo="rtdp", ref=5),
]

horizon = 50

# Algorithms


def algo_aft20(model):
    eps = 0.01

    mdp = Compiler(model).mdp()
    res = util.optimize_and_evaluate(mdp, horizon=horizon, eps=eps)

    return dict(rpp=res["rpp"])


def algo_rtdp(model):
    steps = 300000
    eps = 0.3

    agent = RTDP(model, eps=eps, eps_honest=0, horizon=horizon)
    for i in range(steps):
        agent.step()

    value, progress = agent.start_value_and_progress()
    return dict(value=value, progress=progress, rpp=value / progress)


# TODO, aft20 algo in util calculates steady state rewards, the rtdp one does not. This is apples to oranges.


# How do we instantiate the models and run the algo?


def instanciate_model(*args, model, protocol, truncated, alpha, gamma, **kwargs):
    if model in ["fc16", "aft20"]:
        assert protocol == "bitcoin", "fc16 and aft20 model are bitcoin-only"

    common = dict(alpha=alpha, gamma=gamma)

    if model == "fc16" and truncated:
        return fc16sapirshtein.BitcoinSM(**common, maximum_fork_length=40)

    if model == "fc16" and not truncated:
        return fc16sapirshtein.BitcoinSM(
            **common, maximum_fork_length=10000
        )  # TODO, disable truncation completely

    if model == "aft20" and truncated:
        return aft20barzur.BitcoinSM(**common, maximum_fork_length=40)

    if model == "aft20" and not truncated:
        return aft20barzur.BitcoinSM(
            **common, maximum_fork_length=10000
        )  # TODO, disable truncation completely

    raise ValueError(f"unknown model: {model}")


def measure_unsafe(*args, algo, **kwargs):
    model = instanciate_model(**kwargs)

    if algo == "aft20":
        return algo_aft20(model)
    if algo == "rtdp":
        return algo_rtdp(model)

    raise ValueError(f"unknown algo: {algo}")


def measure(*args, **kwargs):
    try:
        return measure_unsafe(*args, **kwargs)
    except Exception as e:
        return dict(error=str(e), traceback=traceback.format_exc())


# Command line arguments
argp = argparse.ArgumentParser()
argp.add_argument("-j", "--n_jobs", type=int, default=1, metavar="INT")
args = argp.parse_args()


# Multicore measurement loop


def job(*args, **kwargs):
    start_time = time()
    result = measure(**kwargs)
    return kwargs | result | dict(time=time() - start_time)


def job_gen():
    for r in rows:
        for c in columns:
            yield joblib.delayed(job)(**r, **c)


jobs = list(job_gen())
jobs = random.sample(jobs, len(jobs))

res_gen = joblib.Parallel(n_jobs=args.n_jobs, return_as="generator")(jobs)

print()
print("Start solving the MDPs for various parameter combinations:")

rows = []
for res in tqdm(res_gen, total=len(jobs)):
    rows.append(res)

df = pandas.DataFrame(rows)

# Error handling

if "error" in df.columns:
    for idx, r in df[~df.error.isna()].iterrows():
        print(r)
        print(r.traceback)
        print()

    raise Exception("errors during measurements")


def tabulate(df, key):
    return (
        df.pivot(
            columns=["attacker", "alpha", "gamma"],
            index=["row", "protocol", "model", "truncated", "algo", "ref"],
            values=key,
        )
        .reset_index()
        .set_index(["row"])
    )


print()
print("rpp")
print(tabulate(df, "rpp"))
print()
print("time")
print(tabulate(df, "time"))
