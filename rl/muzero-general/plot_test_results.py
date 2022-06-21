import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

if __name__ == "__main__":
    df = pd.read_pickle("test_result.pkl")
    gb_mean = df.groupby("alpha").mean().reset_index()
    gb_std = df.groupby("alpha").std().reset_index()
    plt.scatter(gb_mean["alpha"], gb_mean["total_reward"], c="b")
    plt.errorbar(
        gb_mean["alpha"],
        gb_mean["total_reward"],
        yerr=gb_std["total_reward"],
        fmt="none",
        ecolor="b",
    )
    plt.scatter(gb_mean["alpha"], gb_mean["alpha"], c="r")
    plt.xticks(np.arange(0.05, 0.5, 0.05))
    plt.show()
