import mdp
from bitcoin import Bitcoin
from parallel import Parallel
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
            n_states_explored=explorer.states_explored,
            n_states_queued=explorer.queue.qsize(),
            n_actions_used=len(explorer.action_map),
            transitions_explored=explorer.nonreset_transitions
            + explorer.reset_transitions,
            max_actions=explorer.max_actions,
            distance_time=s.distance_time,
            distance_step=s.distance_step,
            ram_usage_gb=process.memory_info().rss / 1024**3,
        )
        info["queuing_factor"] = info["n_states_queued"] / info["n_states_explored"]
        info["transitions_reset_ratio"] = (
            explorer.reset_transitions / info["transitions_explored"]
        )
        #  pp.pprint(info)
    print(f"{config.protocol.name}: {explorer.states_explored}")


explore_to_end(bitcoin)
explore_to_end(parallel2)
explore_to_end(parallel3)
explore_to_end(parallel4)
explore_to_end(parallel5)
