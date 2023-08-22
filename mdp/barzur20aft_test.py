import barzur20aft
from compiler import Compiler
import pprint
import psutil

pp = pprint.PrettyPrinter(indent=2)


def peek(c):
    x = c.queue.get()
    c.queue.put(x)
    return x


def compile(*args, verbose=False, **kwargs):
    model = barzur20aft.Bitcoin(*args, **kwargs)
    c = Compiler(model)
    while c.explore(steps=10000):
        if verbose:
            process = psutil.Process()
            trace, _state = peek(c)
            info = dict(
                n_states_explored=len(c.explored),
                n_states_queued=c.queue.qsize(),
                n_states_seen=len(c.state_map),
                n_actions=len(c.action_map),
                n_transitions=c._mdp.n_transitions,
                ram_usage_gb=process.memory_info().rss / 1024**3,
            )
            info["queuing_factor"] = info["n_states_queued"] / info["n_states_explored"]
            pp.pprint(info)
    mdp = c.mdp()
    print(
        f"mfl {kwargs['maximum_fork_length']}: {mdp.n_states} states "
        f"and {mdp.n_transitions} transitions"
    )
    return mdp


mdp = compile(alpha=0.25, gamma=0.5, maximum_fork_length=95, verbose=True)
for i in range(10, 20):
    compile(alpha=0.25, gamma=0.5, maximum_fork_length=i)

ptmdp = barzur20aft.ptmdp(mdp, horizon=1000)
