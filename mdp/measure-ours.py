from time import time
from tqdm import tqdm
import barzur20aft
import gzip
import joblib
import pandas
import pickle
import random
import sm


# We start from my already explored Bitcoin/SM models.

fname = "explored-models/models.pkl.gz"
print(f"Loading model list from {fname} and select suitable subset.")

with gzip.open(fname, "rb") as f:
    models = pickle.load(f)

filter = "n_transitions < 1_000_000"
#  filter = 'n_transitions < 2_000' # shortcut for testing
idx = models.query(filter).groupby("protocol").maximum_size.idxmax()
models = models.iloc[idx].reset_index(drop=True)

print()
print(models)


def measure(mdp, value_eps=0.01, alpha=0.25, gamma=0.25, horizon=100):
    start = time()
    mapped_mdp = sm.map_params(mdp, alpha=alpha, gamma=gamma)
    ptmdp = barzur20aft.ptmdp(mapped_mdp, horizon=horizon)
    res = ptmdp.value_iteration(value_eps=value_eps)

    rew = 0.0
    for state, prob in mdp.start.items():
        rew += prob * res["value"][state]

    return dict(vi_start_value=rew, vi_iter=res["iter"], vi_time=time() - start)


def job(row, **kwargs):
    with gzip.open(f"explored-models/{row.key}.pkl.gz", "rb") as f:
        d = pickle.load(f)
    res = measure(d["mdp"], **kwargs)
    return dict(row) | kwargs | res


def job_gen():
    for h in [100]:
        for g in [0, 0.5, 1]:
            for a in range(0, 50, 5):
                for _, row in models.iterrows():
                    yield joblib.delayed(job)(
                        row, alpha=a / 100, gamma=g, horizon=h, value_eps=0.01
                    )


jobs = list(job_gen())
jobs = random.sample(jobs, len(jobs))

res_gen = joblib.Parallel(n_jobs=6, return_as="generator")(jobs)

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
