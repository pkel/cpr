from matplotlib.backends.backend_pdf import PdfPages
import pickle
import seaborn as sns

print()
print("open measure-validation.pkl")

with open("measure-validation.pkl", "rb") as pkl:
    load = pickle.load(pkl)
    fc16 = load["fc16"]
    our = load["our"]
    data = load["data"]

defvar = dict()
defvar["maximumHeight"] = our["model"].maximum_height
defvar["ourStates"] = our["mdp"].n_states
defvar["ourActions"] = our["mdp"].n_actions
defvar["ourTransitions"] = our["mdp"].n_transitions
defvar["fcStates"] = fc16["mdp"].n_states
defvar["fcActions"] = fc16["mdp"].n_actions
defvar["fcTransitions"] = fc16["mdp"].n_transitions

data = data.query("horizon == horizon.max() and eps == eps.min()")
data = data.assign(gamma_percent=(data.gamma * 100).map(int))

defvar["horizon"] = data.horizon.max()
defvar["eps"] = data.eps.min()

data = data.query("gamma in [0., 0.5, 1]")

models = dict(our="Proposed", fc16="Traditional")
data = data.query("model in ['our', 'fc16']")
data = data.assign(model_hum=data.model.map(lambda x: models[x]))

print("plot figure into tab-validation.pdf")
pp = PdfPages("tab-validation.pdf")
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

print("write tex variables to tab-validation.tex")
with open("tab-validation.tex", "w") as f:
    print("%%", file=f)
    print("%% file generated from tab-validation.pdf", file=f)
    print("%%", file=f)

    for cmd, val in defvar.items():
        print(f"\\def\\{cmd}{{{val}}}", file=f)
