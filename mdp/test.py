from compiler import Compiler, StateEditor
from sm import Config, SelfishMining
from bitcoin import Bitcoin
from parallel import Parallel
import mdptoolbox
import pprint
import psutil

pp = pprint.PrettyPrinter(indent=2)


def cfg(protocol, *args, **kwargs):
    return Config(
        protocol=protocol(*args, **kwargs), alpha=0.25, gamma=0.5, truncate_on_pow=5
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
                n_transitions=len(c.transitions),
                trace_blocks_mined=trace.blocks_mined,
                trace_actions_taken=trace.actions_taken,
                ram_usage_gb=process.memory_info().rss / 1024**3,
            )
            info["queuing_factor"] = info["n_states_queued"] / info["n_states_explored"]
            pp.pprint(info)
    print(f"{config.protocol.name}: {len(c.explored)} / {len(c.transitions)}")
    return c.mdp_matrices()


p, r = compile(Bitcoin, verbose=True)
p, r = compile(Parallel, k=2)

VI = mdptoolbox.mdp.ValueIteration(p, r, 1)
VI.setVerbose()
VI.run()
