import cached
import io
import joblib
import matplotlib.pyplot as plt
import numpy as np
import os
import skopt
import skopt.plots
import warnings
from skopt.space import Real
from skopt.utils import use_named_args

memory = joblib.Memory(os.path.dirname(__file__) + "/_cache", verbose=0)


def png_of_optimization(res, break_even, title):
    fig = plt.figure()
    ax = skopt.plots.plot_gaussian_process(res)
    ax.axvline(break_even, linestyle="solid", label="minimum/break-even")
    ax.axvline(res.x[0], linestyle="dashed", label="minimal observation")
    ax.legend(loc="best", numpoints=1)
    ax.set_xlabel("α")
    ax.set_ylabel("f(α)")
    if title:
        ax.set_title(title)
    buf = io.BytesIO()
    fig.savefig(buf, format="png")
    plt.close()
    buf.seek(0)
    return buf.getvalue()


def predicted_minimum(res, support):
    # res.x gives argmin of observation
    # the observations are stochastic, hence we cannot use res.x
    # revert last surrogate function instead
    # surrogate has support 0..1, thus transform alphas
    return res.x_iters[
        np.argmin(
            res.models[-1].predict(
                (np.array(res.x_iters) - min(support)) / (max(support) - min(support))
            )
        )
    ][0]


def find(measure, n_calls=42, support=[0.1, 0.5], title=None, **kwargs):
    dimensions = [Real(name="alpha", low=min(support), high=max(support))]
    if "alpha" in kwargs.keys():
        kwargs["pretend_alpha"] = kwargs["alpha"]
        kwargs.pop("alpha")

    # define objective function
    @use_named_args(dimensions=dimensions)
    def objective(alpha):
        r = measure(alpha=alpha, **kwargs)
        return np.abs(
            (r["episode_reward_attacker"] / r["episode_progress"] / alpha) - 1
        )

    # run optimization
    with warnings.catch_warnings():
        warnings.filterwarnings(
            "ignore", "The objective has been evaluated at this point before"
        )
        res = skopt.gp_minimize(objective, dimensions, n_calls=n_calls)
    # used estimated minimum, not observed minimum
    break_even = predicted_minimum(res, support)
    # optimizer result cannot be pickled
    # thus create plot here and save in dataframe
    png = png_of_optimization(res, break_even, title)
    return dict(break_even=break_even, break_even_png=png)


@memory.cache
def find_hardcoded(env_name, path, **kwargs):
    def measure(**kwargs):
        return cached.measure_hardcoded.func(env_name, path, **kwargs)

    return find(measure, **kwargs)


@memory.cache
def find_trained(env_name, policy, **kwargs):
    def measure(**kwargs):
        return cached.measure_trained.func(env_name, policy, **kwargs)

    return find(measure, **kwargs)
