from bitcoin import Bitcoin
from compiler import Compiler
from parallel import Parallel
from sm_v1 import Config, SelfishMining, StateEditor
import pickle
import pprint
import psutil
import sympy

pp = pprint.PrettyPrinter(indent=2)


def cfg_sym(protocol, *args, **kwargs):
    return Config(
        protocol=protocol(*args, **kwargs),
        alpha=sympy.Symbol("α"),
        gamma=sympy.Symbol("γ"),
        truncate_on_pow=5,
        horizon=sympy.Symbol("H"),
    )


def cfg(protocol, *args, **kwargs):
    return Config(
        protocol=protocol(*args, **kwargs),
        alpha=0.35,
        gamma=0.5,
        truncate_on_pow=5,
        horizon=1000,
    )


def peek(c):
    x = c.queue.get()
    c.queue.put(x)
    return x


def compile(*args, verbose=False, **kwargs):
    config = cfg(*args, **kwargs)
    se = StateEditor()
    c = Compiler(SelfishMining(se, config))
    while c.explore():
        if verbose:
            process = psutil.Process()
            trace, _state = peek(c)
            info = dict(
                protocol=config.protocol.name,
                n_states_explored=len(c.explored),
                n_states_queued=c.queue.qsize(),
                n_states_seen=len(c.state_map),
                n_actions=len(c.action_map),
                n_transitions=c.transitions,
                trace_blocks_mined=trace.blocks_mined,
                trace_actions_taken=trace.actions_taken,
                ram_usage_gb=process.memory_info().rss / 1024**3,
            )
            info["queuing_factor"] = info["n_states_queued"] / info["n_states_explored"]
            pp.pprint(info)
    fname = f"{config.protocol.name}.pkl"
    mdp = c.mdp()
    with open(f"{config.protocol.name}.pkl", "wb") as f:
        pickle.dump(mdp, f)
    print(f"{fname}: {mdp.n_states} states and {mdp.n_transitions} transitions")


# compile(Bitcoin, verbose=True)
compile(Bitcoin)
compile(Parallel, k=2)
compile(Parallel, k=3)
compile(Parallel, k=4)
