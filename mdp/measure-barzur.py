from compiler import Compiler
from time import time
from tqdm import tqdm
import barzur20aft
import gzip
import joblib
import pandas
import pickle
import random
import sm


n_jobs = 6

# We start from my already explored Bitcoin/SM models.

fname = "explored-models/models.pkl.gz"
print(f"Loading model list from {fname} and select suitable MDP.")

with gzip.open(fname, "rb") as f:
    our_models = pickle.load(f)

filter = 'protocol == "bitcoin" and n_transitions < 1_000_000'
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

# Now, reproduce this with bar-zur model

start = time()
their_model = barzur20aft.Bitcoin(maximum_fork_length=mh, **barzur20aft.mappable_params)
their_mdp = Compiler(their_model).mdp()
delta = time() - start

print()
print(f"Building a similar, traditional MDP took {delta:.1f} seconds:")
print(their_mdp)

# Next, generate and solve the PT-MDPs for various alphas/gammas


def measure(mdp, map_params, value_eps=0.01, alpha=0.25, gamma=0.25, horizon=100):
    mapped_mdp = map_params(mdp, alpha=alpha, gamma=gamma)
    ptmdp = barzur20aft.ptmdp(mapped_mdp, horizon=horizon)

    vi = ptmdp.value_iteration(value_eps=value_eps)
    policy = vi.pop("vi_policy")
    value = vi.pop("vi_value")

    vi["vi_start_value"] = 0.0
    for s, prob in ptmdp.start.items():
        vi["vi_start_value"] += value[s] * prob

    rpp = mapped_mdp.reward_per_progress(policy, eps=0.001, n_iter=20)

    return vi | rpp


def job(**kwargs):
    a = measure(their_mdp, barzur20aft.map_params, **kwargs)
    b = measure(our_mdp, sm.map_params, **kwargs)
    return [kwargs | dict(model="their") | a, kwargs | dict(model="our") | b]


def job_gen():
    for h in [50, 100, 200]:
        for g in [0, 0.5, 1]:
            for a in range(0, 50, 5):
                yield joblib.delayed(job)(
                    alpha=a / 100, gamma=g, horizon=h, value_eps=0.01
                )


#  shortcut for testing:
# def job_gen():
#     for h in [50]:
#         for g in [0]:
#             for a in [25, 33]:
#                 yield joblib.delayed(job)(alpha = a / 100, gamma = g, horizon = h)

jobs = list(job_gen())
jobs = random.sample(jobs, len(jobs))

res_gen = joblib.Parallel(n_jobs=n_jobs, return_as="generator")(jobs)

print()
print("Start solving the MDPs for various parameter combinations:")

rows = []
for res in tqdm(res_gen, total=len(jobs)):
    rows.extend(res)

df = pandas.DataFrame(rows)

print()
print(df)

fname = "measure-barzur.pkl"
print()
print(f"storing results in {fname}")

results = dict(
    their=dict(mdp=their_mdp, model=their_model),
    our=dict(mdp=our_mdp, model=our_model),
    data=df,
)

with open(fname, "wb") as pkl:
    pickle.dump(results, pkl)
