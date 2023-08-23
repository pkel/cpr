from bitcoin import Bitcoin
from compiler import Compiler
from parallel import Parallel
from sm import Editor, Miner, SelfishMining
import pprint
import psutil

pp = pprint.PrettyPrinter(indent=2)


def test_editor():
    e = Editor(first_miner=Miner.Defender)
    assert e.n == 1
    g = 0
    b1 = e.append({g}, Miner.Attacker)
    assert e.n == 2
    assert len(e.children(g)) == 1 and list(e.children(g))[0] == b1
    assert len(e.parents(b1)) == 1 and list(e.parents(b1))[0] == g


if __name__ == "__main__":
    test_editor()


def compile(proto, verbose=False, alpha=0.33, gamma=0.5, maximum_size=5, **kwargs):
    model = SelfishMining(
        proto, alpha=alpha, gamma=gamma, maximum_size=maximum_size, **kwargs
    )
    c = Compiler(model)
    while c.explore():
        if verbose:
            process = psutil.Process()
            info = dict(
                model=repr(model),
                n_states_explored=len(c.explored),
                n_states_queued=c.queue.qsize(),
                n_states_seen=len(c.state_map),
                n_actions=len(c.action_map),
                n_transitions=c.transitions,
                ram_usage_gb=process.memory_info().rss / 1024**3,
            )
            info["queuing_factor"] = info["n_states_queued"] / info["n_states_explored"]
            pp.pprint(info)
    mdp = c.mdp()
    print(f"{model}: {mdp}")


if __name__ == "__main__":
    # compile(Bitcoin, verbose=True)
    compile(Bitcoin(), maximum_size=4, force_consider_own=False)
    compile(Bitcoin(), maximum_size=4)
    compile(Bitcoin())
    compile(Parallel(k=2))
    compile(Parallel(k=3))
    compile(Parallel(k=4))
