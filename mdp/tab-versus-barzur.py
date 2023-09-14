from matplotlib.backends.backend_pdf import PdfPages
import pickle
import seaborn as sns

print()
print("open measure-barzur.pkl")

with open("measure-barzur.pkl", "rb") as pkl:
    load = pickle.load(pkl)
    their = load["their"]
    our = load["our"]
    data = load["data"]

defvar = dict()
defvar["maximumHeight"] = our["model"].maximum_height
defvar["ourStates"] = our["mdp"].n_states
defvar["ourActions"] = our["mdp"].n_actions
defvar["ourTransitions"] = our["mdp"].n_transitions
defvar["theirStates"] = their["mdp"].n_states
defvar["theirActions"] = their["mdp"].n_actions
defvar["theirTransitions"] = their["mdp"].n_transitions

data = data.query("horizon == horizon.max() and eps == eps.min()")
data = data.assign(gamma_percent=(data.gamma * 100).map(int))

defvar["horizon"] = data.horizon.max()
defvar["eps"] = data.eps.min()

data = data.query("gamma in [0., 0.5, 1]")

models = dict(our="Proposed", their="Traditional")
data = data.assign(model_hum=data.model.map(lambda x: models[x]))

print("plot figure into tab-versus-barzur.pdf")
pp = PdfPages("tab-versus-barzur.pdf")
g = sns.relplot(
    data,
    kind="line",
    x="alpha",
    y="vi_max_value",
    col="gamma",
    hue="model_hum",
    style="model_hum",
    markers=True,
    palette=sns.color_palette("colorblind", 2),
    height=3,
    aspect=1 / 1,
    facet_kws=dict(legend_out=False),
)
sns.move_legend(
    g, "lower left", bbox_to_anchor=(0.08, 0.6), ncol=1, title="Model", frameon=False
)
g.set_titles(r"$\gamma={col_name}$")
g.set(xlabel=r"$\alpha$", ylabel="Revenue")
# fig, ax = plt.subplots()
# for g in sorted(data.gamma.unique()):
#     data.query(f'gamma == {g}').plot(x='alpha', y='vi_max_value', ax=ax, label=g)
# ax.legend(title='$\\gamma$')
# ax.autoscale(tight=True)
# ax.set(xlabel=r'$\alpha$', ylabel=r'revenue')
pp.savefig()
pp.close()

print("write tex variables to tab-versus-barzur.tex")
with open("tab-versus-barzur.tex", "w") as f:
    print("%%", file=f)
    print("%% file generated from tab-versus-barzur.pdf", file=f)
    print("%%", file=f)

    for cmd, val in defvar.items():
        print(f"\\def\\{cmd}{{{val}}}", file=f)
