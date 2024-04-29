from tqdm import tqdm
import argparse
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
print(f"Loading model list from {fname} and select suitable subset.")

with gzip.open(fname, "rb") as f:
    models = pickle.load(f)

filter = f"n_transitions < {args.n_transitions}"
idx = models.query(filter).groupby("protocol").maximum_size.idxmax()
models = models.iloc[idx].reset_index(drop=True)

print()
print(models)


def measure(mdp, *args, eps, alpha=0.25, gamma=0.25, horizon=100):
    mapped_mdp = sm.map_params(mdp, alpha=alpha, gamma=gamma)
    return util.optimize_and_evaluate(mapped_mdp, horizon=horizon, eps=eps)


def job(row, **kwargs):
    with gzip.open(f"explored-models/{row.key}.pkl.gz", "rb") as f:
        d = pickle.load(f)
    res = measure(d["mdp"], **kwargs)
    return dict(row) | kwargs | res


def job_gen():
    for h in [100]:
        for g in [0, 50, 100]:
            for a in range(5, 50, 5):
                for _, row in models.iterrows():
                    yield joblib.delayed(job)(
                        row, alpha=a / 100, gamma=g / 100, horizon=h, eps=0.01
                    )


jobs = list(job_gen())
jobs = random.sample(jobs, len(jobs))

res_gen = joblib.Parallel(n_jobs=args.n_jobs, return_as="generator")(jobs)

print()
print("Start solving the MDPs for various parameter combinations:")

rows = []
for res in tqdm(res_gen, total=len(jobs)):
    rows.append(res)

df = pandas.DataFrame(rows)

print()
print(df)

fname = "measure-ours.pkl"
print()
print(f"storing results in {fname}")

with open(fname, "wb") as pkl:
    pickle.dump(df, pkl)
