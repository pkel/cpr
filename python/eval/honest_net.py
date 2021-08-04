import numpy as np
import pandas as pd

df = pd.read_csv("data/honest_net.tsv", sep="\t")
df = df.loc[:, ~df.columns.str.contains('^Unnamed')]

# C&P from https://stackoverflow.com/a/39513799
def gini(x):
    # (Warning: This is a concise implementation, but it is O(n**2)
    # in time and memory, where n = len(x).  *Don't* pass in huge
    # samples!)

    # Mean absolute difference
    mad = np.abs(np.subtract.outer(x, x)).mean()
    # Relative mean absolute difference
    rmad = mad/np.mean(x)
    # Gini coefficient
    g = 0.5 * rmad
    return g

def parse_array(s):
    try:
        return np.fromstring(s, dtype=float, sep="|")
    except:
        return np.array([float('nan')])

def expand(row):
    compute = parse_array(row.compute)
    weakest = np.argmin(compute)
    strongest = np.argmax(compute)
    d = {}
    def wsg(k, v):
        d[k + '_weakest'] = v[weakest]
        d[k + '_strongest'] = v[strongest]
        d[k + '_gini'] = gini(v)
    rcompute = compute / np.sum(compute)
    wsg('compute', rcompute)
    activations = parse_array(row.activations)
    assert(np.sum(activations) == row.number_activations or row.error)
    ractivations = activations / np.sum(activations)
    wsg('activations', ractivations)
    reward = parse_array(row.reward)
    rreward = reward / np.sum(reward)
    wsg('reward', rreward)
    efficiency = rreward / ractivations
    wsg('efficiency', rreward / ractivations)
    d['activations_compute_gini_delta'] = d['activations_gini'] - d['compute_gini']
    d['reward_activations_gini_delta'] = d['reward_gini'] - d['activations_gini']
    return d

df=df.join(df.apply(expand, axis=1, result_type='expand'))

#  print(df.pivot(index='block_interval', columns=['protocol', 'k', 'incentive_scheme'], values='reward_gini'))
print(df.pivot(index='block_interval', columns=['protocol', 'k', 'incentive_scheme'], values='efficiency_weakest'))
#  print(df.pivot(index='block_interval', columns=['protocol', 'k', 'incentive_scheme'], values='activations_compute_gini_delta'))
print(df[df.protocol=="george"].pivot(index='block_interval', columns=['protocol', 'k', 'incentive_scheme'], values='reward_activations_gini_delta'))

df.to_csv("data/honest_net_expanded.tsv", sep="\t")
