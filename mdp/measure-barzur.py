from compiler import Compiler
from time import time
from tqdm import tqdm
import barzur20aft
import joblib
import numpy
import pandas
import pickle
import sm
import random


# We start from my already explored Bitcoin/SM models.

fname = "measure-bitcoin.pkl"
print(f"Loading our bitcoin models from {fname} ...")

with open(fname, "rb") as pkl:
    our = pickle.load(pkl)

our = [
    (model.maximum_height, t, model, mdp)
    for t, model, mdp in our
    if mdp.n_transitions < 1000000
]
our = sorted(our)

mh, our_time, our_model, our_mdp = our[-1]

del our

print(
    f"Our biggest MDP with less than 1m transitions has maximum_height {mh} "
    f"and took {our_time:.0f} seconds to compile:"
)
print(our_mdp)

# Now, reproduce this with bar-zur model

start = time()
model = barzur20aft.Bitcoin(maximum_fork_length=mh, **barzur20aft.mappable_params)
their_mdp = Compiler(model).mdp()
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

results = dict(their=their_mdp, our=our_mdp, data=df)

with open(fname, "wb") as pkl:
    pickle.dump(results, pkl)
