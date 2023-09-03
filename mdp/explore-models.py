from bitcoin import Bitcoin
from compiler import Compiler
from ethereum import EthereumWhitepaper, EthereumByzantium
from parallel import Parallel
from sm import SelfishMining, mappable_params
from time import time
import argparse
import joblib
import logging
import gzip
import pandas
import pickle
import traceback


argp = argparse.ArgumentParser()
argp.add_argument("-j", "--n_jobs", type=int, default=6, metavar="INT")
argp.add_argument(
    "-t",
    "--n_transitions",
    type=int,
    default=1_000_000,
    metavar="INT",
    help="maximum number of transitions per model",
)
argp.add_argument(
    "-b",
    "--time_budget",
    type=int,
    default=3600,
    metavar="INT",
    help="maximum exploration time per model",
)
args = argp.parse_args()

# Verbose output
log = logging.getLogger("explore-states.py")
logging.basicConfig(
    format="%(asctime)s %(levelname)-8s %(message)s",
    level=logging.INFO,
    datefmt="%Y-%m-%d %H:%M:%S",
)


def model_fn(proto, **kwargs):
    return SelfishMining(proto, **kwargs, **mappable_params)


models = dict()
models["btc-mh"] = lambda x: model_fn(Bitcoin(), maximum_height=x)
models["btc-ms"] = lambda x: model_fn(Bitcoin(), maximum_size=x)
models["eth-2-ms"] = lambda x: model_fn(EthereumWhitepaper(horizon=2), maximum_size=x)
models["eth-3-ms"] = lambda x: model_fn(EthereumWhitepaper(horizon=3), maximum_size=x)
models["byz-2-ms"] = lambda x: model_fn(EthereumByzantium(horizon=2), maximum_size=x)
models["byz-3-ms"] = lambda x: model_fn(EthereumByzantium(horizon=3), maximum_size=x)
models["par-2-ms"] = lambda x: model_fn(Parallel(k=2), maximum_size=x)
models["par-3-ms"] = lambda x: model_fn(Parallel(k=3), maximum_size=x)
models["par-4-ms"] = lambda x: model_fn(Parallel(k=4), maximum_size=x)

scheduled = {k: 1 for k in models.keys()}
done = {k: False for k in models.keys()}


def job(key, model_fn, i):
    start = time()
    stop = start + args.time_budget

    model = model_fn(i)

    try:
        compiler = Compiler(model_fn(i))
        while compiler.explore(steps=1000):
            if time() > stop:
                return key, i, model, "time", None
            if compiler._mdp.n_transitions > args.n_transitions:
                return key, i, model, "size", None
            if done[key]:  # other model (with lower i) aborted
                return key, i, model, "done", None

        mdp = compiler.mdp()

    except AssertionError as e:
        tb = traceback.format_exc()
        return key, i, model, "error", (e, tb)

    delta = time() - start

    return key, i, model, delta, mdp


def jobs():
    while True:
        for key, model_fn in models.items():
            if done[key]:
                continue

            scheduled[key] += 1
            i = scheduled[key]
            log.debug(f"schedule {key}-{i}")
            yield joblib.delayed(job)(key, model_fn, i)

        if all(done.values()):
            break


results = joblib.Parallel(n_jobs=args.n_jobs, batch_size=1, return_as="generator")(
    jobs()
)

meta = []


def consume():
    for key, i, model, t, mdp in results:
        if not isinstance(t, float):
            if t == "time":
                log.info(f"abort {key}-{i}: time limit exceeded")
            elif t == "size":
                log.info(f"abort {key}-{i}: transition limit exceeded")
            elif t == "error":
                msg, tb = mdp
                log.error(f"error in {key}-{i}: {msg}")
                with open(f"explored-models/{key}-{i}.err", "w") as f:
                    f.write(tb)
            elif t == "done":
                pass  # marked as done before
            else:
                raise ValueError(t)
            done[key] = True
        else:
            log.info(f"compiled {key}-{i} in {t:.2f} seconds: {mdp}")

            data = dict(model=model, mdp=mdp, time=t)
            fname = f"explored-models/{key}-{i}.pkl"
            with gzip.open(fname + ".gz", "wb") as pkl:
                pickle.dump(data, pkl)

            meta.append(
                dict(
                    key=f"{key}-{i}",
                    protocol=model.protocol.name,
                    maximum_height=model.maximum_height,
                    maximum_size=model.maximum_size,
                    model_hum=f"{model}",
                    protocol_hum=f"{model.protocol}",
                    time=t,
                    n_states=mdp.n_states,
                    n_actions=mdp.n_actions,
                    n_transitions=mdp.n_transitions,
                )
            )

        if all(done.values()):
            log.info("all done")

            df = pandas.DataFrame(meta)
            print(df)
            with gzip.open("explored-models/models.pkl.gz", "wb") as pkl:
                pickle.dump(df, pkl)

            print()
            print("ignore the following warning")

            # force termination of parallel for-loop
            # raises on next completed task
            break


try:
    consume()
except joblib.externals.loky.process_executor.ShutdownExecutorError:
    # we trigger this intentionally to abort processing
    pass
