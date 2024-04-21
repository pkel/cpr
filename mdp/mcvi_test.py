import aft20barzur
from monte_carlo_value_iteration import MCVI
import pprint
import psutil
import numpy

from bitcoin import Bitcoin
from sm import SelfishMining

pp = pprint.PrettyPrinter(indent=2)


def mcvi(
    model,
    *args,
    horizon=100,
    steps=10000,
    eps=0.1,
    eps_honest=0.1,
    report_steps=None,
    honest_warmup_steps=0,
    **kwargs
):
    if honest_warmup_steps > 0:
        agent = MCVI(model, eps=0, eps_honest=1, horizon=horizon, **kwargs)
    else:
        agent = MCVI(model, eps=eps, eps_honest=eps_honest, horizon=horizon, **kwargs)

    max_start_value = 0

    j = 0
    for i in range(steps):
        agent.step()

        if honest_warmup_steps > 0 and (i >= honest_warmup_steps):
            agent.set_exploration(eps=eps, eps_honest=eps_honest)
            honest_warmup_steps = 0

        j += 1
        if report_steps and j >= report_steps:
            j = 0
            process = psutil.Process()
            state_count_q = numpy.quantile(
                agent.state_count, [0.9, 0.95, 0.99, 0.995, 0.999]
            )
            state_freq_q = state_count_q / (i + 1)
            start_value = agent.start_value()
            assert start_value >= max_start_value
            max_start_value = max(start_value, max_start_value)

            info = dict(
                steps=i + 1,
                episodes=agent.episode,
                mean_progress=agent.mean_progress,
                n_states=len(agent.state_map),
                start_value=start_value,
                #  start_value_max = max_start_value,
                start_value_norm=start_value / horizon,
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
    problem = dict(alpha=0.42, gamma=0.84)

    if True:
        model_a = SelfishMining(
            Bitcoin(), **problem, maximum_size=50, merge_isomorphic=False
        )
        mcvi(
            model_a,
            steps=1000000,
            report_steps=50,
            horizon=30,
            eps=0.3,
            eps_honest=0.1,
            honest_warmup_steps=10000,
        )
    else:
        model_b = aft20barzur.BitcoinSM(**problem, maximum_fork_length=10000)
        mcvi(
            model_b,
            steps=1000000,
            report_steps=10000,
            horizon=30,
            eps=0.3,
            eps_honest=0.0,
            honest_warmup_steps=10000,
        )
