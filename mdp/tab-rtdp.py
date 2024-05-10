import pickle

fname = "measure-rtdp.pkl"
print()
print(f"load results in {fname}")

with open(fname, "rb") as pkl:
    results = pickle.load(pkl)

df = results["data"]

df = df.assign(start_rpp=lambda x: x.start_value / x.start_progress)
df = df.assign(pimc_ss_rpp=lambda x: x.pimc_ss_reward / x.pimc_ss_progress)


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


for col in [
    "start_rpp",
    "mdp_n_states",
    "pimc_ss_rpp",
    "pimc_n_states",
    "ss_value",
    "ss_n_states_reachable",
    "ss_n_states_nonzero",
    "time",
    "ss_time",
]:
    print()
    print(col)
    print(tabulate(df, col))
