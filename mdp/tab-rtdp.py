import pickle

fname = "measure-rtdp.pkl"
print()
print(f"load results in {fname}")

with open(fname, "rb") as pkl:
    results = pickle.load(pkl)

df = results["data"]

df = df.assign(rpp=lambda x: x.value / x.progress)


def tabulate(df, key):
    return (
        df.pivot(
            columns=["attacker", "alpha", "gamma"],
            index=["row", "protocol", "model", "truncated", "algo", "ref"],
            values=key,
        )
        .reset_index()
        .set_index(["row"])
    )


print()
print("rpp")
print(tabulate(df, "rpp"))
print()
print("time")
print(tabulate(df, "time"))
