import pickle

fname = "measure-rtdp.pkl"
print()
print(f"load results in {fname}")

with open(fname, "rb") as pkl:
    results = pickle.load(pkl)

df = results["data"]

df = df.assign(start_rpp=lambda x: x.start_value / x.start_progress)


def tabulate(df, key):
    return (
        df.pivot(
            columns=["attacker", "alpha", "gamma"],
            index=["row", "protocol", "model", "trunc", "algo", "ref"],
            values=key,
        )
        .reset_index()
        .set_index(["row"])
    )


for col in ["start_rpp", "mdp_n_states", "pimc_n_states", "time"]:
    print()
    print(col)
    print(tabulate(df, col))
