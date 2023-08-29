from bitcoin import Bitcoin
from compiler import Compiler
from ethereum import EthereumWhitepaper, EthereumByzantium
from parallel import Parallel
from sm import SelfishMining, mappable_params
from time import time
import os

protocols = [
    Bitcoin(),
    EthereumByzantium(horizon=1),
    EthereumWhitepaper(horizon=1),
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
    models.append(model_fn(p, 6))
    models.append(model_fn(p, 7))

print(f"Benchmarking the parallel generation of {len(models)} SM models.")
print()


print(
    f"There are {os.cpu_count()} cores available on this machine. "
    "I'll increase the number of cores used step-by-step until I reach "
    "diminishing returns."
)
print()


def per_core(*args, verbose=False):
    start = time()
    for m in models:
        if verbose:
            startm = time()
            print(f"Compile {m} ", end="", flush=True)
        _ = Compiler(m).mdp()
        if verbose:
            secm = time() - startm
            print(f"[{secm:.2f} s]")
    sec = time() - start
    return sec


per_core(verbose=True)
