from bitcoin import Bitcoin
from compiler import Compiler
from sm import SelfishMining, mappable_params
from time import time
import pickle


# We set a time budget for exploring the Bitcoin/SM model

budget = 60 * 60  # seconds

bitcoin = Bitcoin()

sstart = time()


def tprint(*args, **kwargs):
    print(f"{time() - sstart:.2f}:", *args, **kwargs)


def compile(mh):
    tprint(f"start compile({mh})")

    start = time()
    stop = start + budget
    model = SelfishMining(bitcoin, maximum_height=mh, **mappable_params)
    compiler = Compiler(model)
    while compiler.explore(steps=5000):
        if time() > stop:
            tprint(f"abort compile({mh})")
            return "timeout", None, None
    mdp = compiler.mdp()
    delta = time() - start
    tprint(f"done compile({mh}) in {delta:.2f} seconds")
    return delta, model, mdp


def gen():
    mh = 2
    while True:
        time, model, mdp = compile(mh)
        if time == "timeout":
            return None
        yield time, model, mdp
        mh += 1


results = list(gen())

fname = "measure-bitcoin.pkl"
print()
print(f"storing results in {fname}")

with open(fname, "wb") as pkl:
    pickle.dump(results, pkl)

print("done")
