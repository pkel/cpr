from compiler import Compiler
from time import time
from tqdm import tqdm
import barzur20aft
import gzip
import joblib
import numpy
import pandas
import pickle
import random
import sm


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


def value_iteration(mdp, *args, n_iter=0, value_eps=0, discount=1, verbose=False):
    value = numpy.zeros(mdp.n_states, dtype=float)
    policy = numpy.zeros(mdp.n_states, dtype=int)

    i = 1
    while True:
        value_next = numpy.zeros(mdp.n_states, dtype=float)
        policy_next = numpy.zeros(mdp.n_states, dtype=int)

        for src, actions in enumerate(mdp.tab):
            best_v = 0.0
            best_a = -1  # no action possible
            for act, lst in actions.items():
                if act < 0:
                    continue
                this_v = 0.0
                for t in lst:
                    this_v += t.probability * (
                        t.reward + discount * value[t.destination]
                    )
                if this_v >= best_v:  # intentionally to not stick with action -1
                    best_v = this_v
                    best_a = act
            value_next[src] = best_v
            policy_next[src] = best_a
            assert best_a >= 0 or len(actions) == 0

        value_delta = numpy.abs(value_next - value).max()
        policy_change = (policy_next != policy).sum() / len(policy) * 100
        if verbose:
            print(
                f"\riteration {i}: value delta {value_delta:g}, "
                f"policy change {policy_change:.2f}%",
                end="",
            )
        value = value_next
        policy = policy_next

        if n_iter > 0 and i >= n_iter:
            break
        elif value_delta <= value_eps:
            break
        else:
            i += 1

    if verbose:
        print()  # new line to finish verbose progress bar

    return value, policy, i


def measure(mdp, map_params, value_eps=0.01, alpha=0.25, gamma=0.25, horizon=100):
    start = time()
    mapped_mdp = map_params(mdp, alpha=alpha, gamma=gamma)
    ptmdp = barzur20aft.ptmdp(mapped_mdp, horizon=horizon)
    value, policy, i = value_iteration(ptmdp, value_eps=value_eps)

    rew = 0.0
    for state, prob in mdp.start.items():
        rew += prob * value[state]

    return dict(start_value=rew, iter=i, time=time() - start)


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

res_gen = joblib.Parallel(n_jobs=6, return_as="generator")(jobs)

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
