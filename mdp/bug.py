from pynauty import *

ad = {0: [], 1: [0], 2: [0], 3: [0]}
vc1 = [{1}, {2, 3}, {0}]
vc2 = [{1, 2}, {3}, {0}]

g1 = Graph(4, True, ad, vc1)
g2 = Graph(4, True, ad, vc2)

if isomorphic(g1, g2):
    print("graphs are isomorphic")
else:
    print("graphs are not isomorphic")

if certificate(g1) == certificate(g2):
    print("certificates are equal")
else:
    print("certificates are different")


def relabel(ad, vc):
    g = Graph(4, True, ad, vc)
    label = canon_label(g)
    adnew = dict()
    for k, v in ad.items():
        adnew[label[k]] = [label[x] for x in v]
    vcnew = []
    for s in vc:
        vcnew.append({label[x] for x in s})
    return Graph(4, True, adnew, vcnew)


if certificate(relabel(ad, vc1)) == certificate(relabel(ad, vc2)):
    print("certificates after relabel are equal")
else:
    print("certificates after relabel are different")
