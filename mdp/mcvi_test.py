import aft20barzur
from monte_carlo_value_iteration import MCVI
import pprint
import psutil
import numpy

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
            state_count_q = numpy.quantile(
                agent.state_count, [0.9, 0.95, 0.99, 0.995, 0.999]
            )
            state_freq_q = state_count_q / (i + 1)
            info = dict(
                steps=i + 1,
                episodes=agent.episode,
                mean_progress=agent.mean_progress,
                n_states=len(agent.state_map),
                start_value=agent.start_value(),
                start_value_norm=agent.start_value() / horizon,
                ram_usage_gb=process.memory_info().rss / 1024**3,
                state_revisit=(i + 1 - len(agent.state_map)) / (i + 1),
                state_count_q900=state_count_q[0],
                state_count_q950=state_count_q[1],
                state_count_q990=state_count_q[2],
                state_count_q995=state_count_q[3],
                state_count_q999=state_count_q[4],
                state_freq_q900=state_freq_q[0],
                state_freq_q950=state_freq_q[1],
                state_freq_q990=state_freq_q[2],
                state_freq_q995=state_freq_q[3],
                state_freq_q999=state_freq_q[4],
            )
            pp.pprint(info)


def test_mcvi(*args, **kwargs):
    model = aft20barzur.BitcoinSM(alpha=0.42, gamma=0.84, maximum_fork_length=10000)
    mcvi(model, *args, **kwargs)


if __name__ == "__main__":
    model = SelfishMining(
        Bitcoin(), alpha=0.30, gamma=1, maximum_size=20, merge_isomorphic=False
    )
    #  model = aft20barzur.BitcoinSM(alpha=0.30, gamma=1, maximum_fork_length=10000)
    mcvi(model, steps=1000000, report_steps=50, horizon=30)
