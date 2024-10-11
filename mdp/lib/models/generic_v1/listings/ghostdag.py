k: int = ...  # protocol parameter


def mining():
    return tips(G)


def history_of(G):  # eprint.iacr.org/2018/104.pdf ; Alg. 1
    if len(G) == 1:
        return ({genesis}, [genesis])

    blue, hist = dict(), dict()
    for b in tips(G):
        blue[b], hist[b] = history_of(past(G, b))
    b_max = max((len(blue[b]), hash(b), b) for b in tips(G))[2]
    blue = blue[b_max] | {b_max}
    hist = hist[b_max] + [b_max]

    for b in topological_order(anticone(G, b_max)):
        if is_k_cluster(G, blue | {b}):
            blue = blue | {b}
            hist = hist + [b]  # only blue blocks get a reward

    return (blue, hist)


def is_k_cluster(G, S):
    return all(len(anticone(G, b) & S) <= k for b in S)


def history():
    _blue, history = history_of(G)
    return history


# init() and fn update() do nothing
# progress() and coinbase() are like in Bitcoin
