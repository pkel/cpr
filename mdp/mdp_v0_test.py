import mdp_v0 as mdp
from bitcoin import Bitcoin
from parallel import Parallel
import mdptoolbox
import pprint
import psutil

pp = pprint.PrettyPrinter(indent=2)

bitcoin = mdp.Config(protocol=Bitcoin(), alpha=0.25, gamma=0.5, stop_time=6)
parallel2 = mdp.Config(protocol=Parallel(k=2), alpha=0.25, gamma=0.5, stop_time=6)
parallel3 = mdp.Config(protocol=Parallel(k=3), alpha=0.25, gamma=0.5, stop_time=6)
parallel4 = mdp.Config(protocol=Parallel(k=4), alpha=0.25, gamma=0.5, stop_time=6)
parallel5 = mdp.Config(protocol=Parallel(k=5), alpha=0.25, gamma=0.5, stop_time=6)


def explore_to_end(config):
    explorer = mdp.Explorer(config)
    while explorer.explore():
        process = psutil.Process()
        s = explorer.peek()
        info = dict(
            n_states_seen=len(explorer.state_map),
            n_states_queued=explorer.queue.qsize(),
            transitions_explored=explorer.nonreset_transitions
            + explorer.reset_transitions,
            n_states=explorer.n_states,
            n_actions=explorer.n_actions,
            distance_time=s.distance_time,
            distance_step=s.distance_step,
            ram_usage_gb=process.memory_info().rss / 1024**3,
        )
        info["queuing_factor"] = info["n_states_queued"] / info["n_states"]
        info["transitions_reset_ratio"] = (
            explorer.reset_transitions / info["transitions_explored"]
        )
        #  pp.pprint(info)
    print(f"{config.protocol.name}: {explorer.n_states} / {len(explorer.transitions)}")
    return explorer.mdp_matrices()


p, r = explore_to_end(bitcoin)
explore_to_end(parallel2)
explore_to_end(parallel3)
explore_to_end(parallel4)
#  explore_to_end(parallel5)

VI = mdptoolbox.mdp.ValueIteration(p, r, 1)
VI.setVerbose()
VI.run()
