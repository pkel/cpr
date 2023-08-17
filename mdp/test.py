from compiler import Compiler, StateEditor
from sm import Config, SelfishMining
from bitcoin import Bitcoin
from parallel import Parallel
import mdptoolbox
import pprint
import psutil

pp = pprint.PrettyPrinter(indent=2)

bitcoin = Config(protocol=Bitcoin(), alpha=0.25, gamma=0.5)
parallel2 = Config(protocol=Parallel(k=2), alpha=0.25, gamma=0.5)
parallel3 = Config(protocol=Parallel(k=3), alpha=0.25, gamma=0.5)
parallel4 = Config(protocol=Parallel(k=4), alpha=0.25, gamma=0.5)
parallel5 = Config(protocol=Parallel(k=5), alpha=0.25, gamma=0.5)


def peek(c):
    x = c.queue.get()
    c.queue.put(x)
    return x


def compile(config):
    se = StateEditor()
    c = Compiler(SelfishMining(se, config))
    while c.explore():
        process = psutil.Process()
        trace, _state = peek(c)
        info = dict(
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


p, r = compile(bitcoin)

#  p, r = explore_to_end(bitcoin)
#  explore_to_end(parallel2)
#  explore_to_end(parallel3)
#  explore_to_end(parallel4)
#  explore_to_end(parallel5)

VI = mdptoolbox.mdp.ValueIteration(p, r, 1)
VI.setVerbose()
VI.run()
