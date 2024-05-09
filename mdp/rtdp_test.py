import aft20barzur
from rtdp import RTDP
import pprint
import psutil
import sys

from bitcoin import Bitcoin
from sm import SelfishMining

pp = pprint.PrettyPrinter(indent=2)


def rtdp(
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
        agent = RTDP(model, eps=0, eps_honest=1, horizon=horizon, **kwargs)
    else:
        agent = RTDP(model, eps=eps, eps_honest=eps_honest, horizon=horizon, **kwargs)

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

            start_value, start_progress = agent.start_value_and_progress()
            assert start_value >= max_start_value, "value iteration is monotonic"
            max_start_value = max(start_value, max_start_value)

            # calculate average state size
            acc = sum((sys.getsizeof(s) for s in agent.states.keys()))
            avg_size = acc / len(agent.states)

            info = dict(
                steps=i + 1,
                n_episodes=agent.n_episodes,
                progress_gamma999=agent.progress_gamma999,
                n_states=len(agent.states),
                n_states_visited=agent.n_states_visited,
                n_states_exploring_starts=len(agent.exploring_starts),
                start_value=start_value,
                start_value_by_horizon=start_value / horizon,
                start_value_by_progress=start_value / start_progress,
                start_progress=start_progress,
                ram_usage_gb=process.memory_info().rss / 1024**3,
                exploration_states_per_step=len(agent.states) / (i + 1),
                exploration_gamma9999=agent.exploration_gamma9999,
                state_size_hashed_avg=avg_size,
                state_size_gamma9999=agent.state_size_gamma9999,
            )
            pp.pprint(info)

    mdp, policy = agent.mdp_and_policy()


def test_rtdp(*args, **kwargs):
    model = aft20barzur.BitcoinSM(alpha=0.42, gamma=0.84, maximum_fork_length=10000)
    rtdp(model, *args, **kwargs)


if __name__ == "__main__":
    problem = dict(alpha=0.42, gamma=0.84)

    if True:
        model_a = SelfishMining(
            Bitcoin(), **problem, maximum_size=10000, merge_isomorphic=False
        )
        rtdp(
            model_a,
            steps=10000000,
            report_steps=1000,
            horizon=30,
            eps=0.2,
            eps_honest=0.2,
            honest_warmup_steps=10000,
        )
    else:
        model_b = aft20barzur.BitcoinSM(**problem, maximum_fork_length=10000)
        rtdp(
            model_b,
            steps=1000000,
            report_steps=10000,
            horizon=30,
            eps=0.3,
            eps_honest=0.0,
            honest_warmup_steps=10000,
        )
