import aft20barzur
from monte_carlo_value_iteration import MCVI
import pprint
import psutil

from bitcoin import Bitcoin
from sm import SelfishMining

pp = pprint.PrettyPrinter(indent=2)


def mcvi(model, *args, horizon=100, steps=10000, eps=0.1, report_steps=None, **kwargs):
    agent = MCVI(model, eps=eps, horizon=horizon)

    j = 0
    for i in range(steps):
        agent.step()

        j += 1
        if report_steps and j >= report_steps:
            j = 0
            process = psutil.Process()
            info = dict(
                steps=i + 1,
                episodes=agent.episode,
                mean_progress=agent.mean_progress,
                n_states=len(agent.state_map),
                start_value=agent.start_value(),
                start_value_norm=agent.start_value() / horizon,
                ram_usage_gb=process.memory_info().rss / 1024**3,
            )
            pp.pprint(info)


def test_mcvi(*args, **kwargs):
    model = aft20barzur.BitcoinSM(alpha=0.42, gamma=0.84, maximum_fork_length=10000)
    mcvi(model, *args, **kwargs)


if __name__ == "__main__":
    model = SelfishMining(
        Bitcoin(), alpha=0.30, gamma=1, maximum_size=60, merge_isomorphic=False
    )
    #  model = aft20barzur.BitcoinSM(alpha=0.30, gamma=1, maximum_fork_length=10000)
    mcvi(model, steps=1000000, report_steps=50, horizon=30)
