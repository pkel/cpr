from lib.models.generic_v0.bitcoin import Bitcoin
from lib.models.generic_v0.ethereum import EthereumWhitepaper, EthereumByzantium
from lib.models.generic_v0.parallel import Parallel
from lib.models.generic_v0.model import SelfishMining, mappable_params
from lib.compiler import Compiler
from time import time
import joblib
import os
import random

protocols = [
    Bitcoin(),
    EthereumByzantium(horizon=2),
    EthereumWhitepaper(horizon=2),
    EthereumByzantium(horizon=3),
    EthereumWhitepaper(horizon=3),
    Parallel(k=2),
    Parallel(k=3),
    Parallel(k=4),
]


def model_fn(p, ms=7):
    m = SelfishMining(p, maximum_size=ms, **mappable_params)
    return m


models = []
for p in protocols:
    models.append(model_fn(p, 5))
    models.append(model_fn(p, 6))
    models.append(model_fn(p, 7))

print(f"Benchmarking the parallel generation of {len(models)} SM models.")
print()


print(f"This machine has {os.cpu_count()} cores.")
print()


def per_core(*args, verbose=False, shuffle=True):
    jobs = models
    if shuffle:
        jobs = random.sample(jobs, len(jobs))
    start = time()
    for m in jobs:
        if verbose:
            startm = time()
            print(f"Compile {m} ", end="", flush=True)
        _ = Compiler(m).mdp()
        if verbose:
            secm = time() - startm
            print(f"[{secm:.2f} s]")
    sec = time() - start
    return sec


results = [per_core(verbose=True, shuffle=False)]

print()
print(f"1 core:  {results[-1]:.2f} seconds (one unit of work)")


def multicore(n_jobs):
    start = time()
    _ = joblib.Parallel(n_jobs=n_jobs)(
        joblib.delayed(per_core)() for _ in range(n_jobs)
    )
    sec = time() - start
    return sec / n_jobs


for i in range(2, os.cpu_count() + 1):
    print(f"{i} cores: ", end="", flush=True)
    results.append(multicore(i))
    print(f"{results[-1]:.2f} seconds (per unit of work)")
