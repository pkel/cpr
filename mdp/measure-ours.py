from tqdm import tqdm
import argparse
import barzur20aft
import gzip
import joblib
import numpy
import pandas
import pickle
import random
import sm

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
    # Modify standard stopping condition (see mdp.py) assuming
    # 1 - 1/horizon = discount.
    # Also PTO values will be in the range 0 .. horizon.
    # Evaluation metric reward per progress is in 0 .. 1.
    # We want eps-optimality in eval space and thus scale the stop_delta
    # accordingly.
    discount = 1 - 1 / horizon
    delta = (eps * horizon) * (1 - discount) / discount

    mapped_mdp = sm.map_params(mdp, alpha=alpha, gamma=gamma)
    ptmdp = barzur20aft.ptmdp(mapped_mdp, horizon=horizon)

    vi = ptmdp.value_iteration(stop_delta=delta, eps=None, discount=1)

    policy = vi.pop("vi_policy")
    value = vi.pop("vi_value")

    vi["vi_start_value"] = 0.0
    for s, prob in ptmdp.start.items():
        vi["vi_start_value"] += value[s] * prob

    best_state = numpy.argmax(value)
    vi["vi_max_value"] = value[best_state]

    mc = mapped_mdp.markov_chain(policy, start_state=best_state)
    ss = mapped_mdp.steady_state(mc["prb"])
    ss_vec = ss.pop("ss")

    rpp = mapped_mdp.reward_per_progress(
        policy, **mc, ss=ss_vec, eps=eps, min_iter=0, max_iter=20
    )

    return vi | ss | rpp


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
                        row, alpha=a / 100, gamma=g / 100, horizon=h, eps=0.001
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
