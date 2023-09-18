from compiler import Compiler
from time import time
from tqdm import tqdm
import aft20barzur
import argparse
import fc16sapirshtein
import gzip
import joblib
import pandas
import pickle
import random
import sm
import util

argp = argparse.ArgumentParser()
argp.add_argument("-j", "--n_jobs", type=int, default=6, metavar="INT")
argp.add_argument(
    "-t",
    "--n_transitions",
    type=int,
    default=1_000_000,
    metavar="INT",
    help="filter models for maximum transition count",
)
args = argp.parse_args()

# We start from my already explored Bitcoin/SM models.

fname = "explored-models/models.pkl.gz"
print(f"Loading model list from {fname} and select suitable MDP.")

with gzip.open(fname, "rb") as f:
    our_models = pickle.load(f)

filter = f'protocol == "bitcoin" and n_transitions < {args.n_transitions}'
our_idx = our_models.query(filter).maximum_height.idxmax()
our = our_models.iloc[our_idx]

print()
print(our)

fname = f"explored-models/{our.key}.pkl.gz"
print()
print(f"load model from {fname}")

with gzip.open(fname, "rb") as f:
    our = pickle.load(f)

our_model = our["model"]
our_mdp = our["mdp"]
mh = our_model.maximum_height

# Now, reproduce this with traditional models

start = time()
fc16_model = fc16sapirshtein.BitcoinSM(
    maximum_fork_length=mh, **fc16sapirshtein.mappable_params
)
aft20_model = aft20barzur.BitcoinSM(
    maximum_fork_length=mh, **aft20barzur.mappable_params
)
fc16_mdp = Compiler(fc16_model).mdp()
aft20_mdp = Compiler(aft20_model).mdp()
delta = time() - start

print()
print(f"Building a similar, traditional MDPs took {delta:.1f} seconds:")
print("fc16 ", fc16_mdp)
print("aft20", aft20_mdp)

# Next, generate and solve the PT-MDPs for various alphas/gammas


def measure(mdp, map_params, *args, eps, alpha=0.25, gamma=0.25, horizon=100):
    mapped_mdp = map_params(mdp, alpha=alpha, gamma=gamma)
    return util.optimize_and_evaluate(mapped_mdp, horizon=horizon, eps=eps)


def job(**kwargs):
    a = measure(fc16_mdp, fc16sapirshtein.map_params, **kwargs)
    b = measure(aft20_mdp, aft20barzur.map_params, **kwargs)
    c = measure(our_mdp, sm.map_params, **kwargs)
    return [
        kwargs | dict(model="fc16") | a,
        kwargs | dict(model="aft20") | b,
        kwargs | dict(model="our") | c,
    ]


def job_gen():
    for e in [0.01]:
        for h in [25, 50, 100]:
            for g in range(0, 101, 25):
                for a in range(5, 50, 5):
                    yield joblib.delayed(job)(
                        alpha=a / 100, gamma=g / 100, horizon=h, eps=e
                    )


#  shortcut for testing:
# def job_gen():
#     for h in [50]:
#         for g in [0]:
#             for a in [25, 33]:
#                 yield joblib.delayed(job)(alpha = a / 100, gamma = g, horizon = h)

jobs = list(job_gen())
jobs = random.sample(jobs, len(jobs))

res_gen = joblib.Parallel(n_jobs=args.n_jobs, return_as="generator")(jobs)

print()
print("Start solving the MDPs for various parameter combinations:")

rows = []
for res in tqdm(res_gen, total=len(jobs)):
    rows.extend(res)

df = pandas.DataFrame(rows)

print()
print(df)

fname = "measure-validation.pkl"
print()
print(f"storing results in {fname}")

results = dict(
    fc16=dict(mdp=fc16_mdp, model=fc16_model),
    aft20=dict(mdp=aft20_mdp, model=aft20_model),
    our=dict(mdp=our_mdp, model=our_model),
    data=df,
)

with open(fname, "wb") as pkl:
    pickle.dump(results, pkl)
