import numpy as np
import pandas as pd

df = pd.read_csv("data/honest_net.tsv", sep="\t")

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
    return np.fromstring(s, dtype=float, sep="|")

def expand(row):
    compute = parse_array(row.compute)
    weakest = np.argmin(compute)
    strongest = np.argmax(compute)
    d = {}
    def wsg(k, v):
        d[k + '.weakest'] = v[weakest]
        d[k + '.strongest'] = v[strongest]
        d[k + '.gini'] = gini(v)
    rcompute = compute / np.sum(compute)
    wsg('compute', rcompute)
    reward = parse_array(row.reward)
    rreward = reward / np.sum(reward)
    wsg('reward', rreward)
    efficiency = rreward / rcompute
    wsg('efficiency', rreward / rcompute)
    d['reward.gini.delta'] = d['reward.gini'] - d['compute.gini']
    return d

df=df.join(df.apply(expand, axis=1, result_type='expand'))

#  print(df.pivot(index='block-interval', columns=['protocol', 'k', 'incentive-scheme'], values='reward.gini'))
#  print(df.pivot(index='block-interval', columns=['protocol', 'k', 'incentive-scheme'], values='efficiency.weakest'))
print(df.pivot(index='block-interval', columns=['protocol', 'k', 'incentive-scheme'], values='reward.gini.delta'))
