import numpy
import pickle

protocol = "bitcoin"

with open(f"{protocol}.pkl", "rb") as f:
    P, R = pickle.load(f)

# Let's do a naive value iteration according to
# Wikipedia:Markov Decision Process

A = len(P)  # number of actions
S = P[0].shape[0]  # number of states

print(f"{protocol} protocol, {S} states, {A} actions")

V = numpy.zeros((1, S), dtype=float)
policy = numpy.zeros(S, dtype=int)
discount = 0.99

for iteration in range(100):
    Vnext = numpy.zeros((1, S))
    policy_next = numpy.zeros(S, dtype=int)
    for s in range(S):
        best = 0.0
        for a in range(A):
            # The + V part becomes dense after some iterations
            option = P[a][s].multiply(R[a][s] + (discount * V)).sum()
            if option > best:
                policy_next[s] = a
                best = option
        Vnext[0, s] = best
    Vdelta = numpy.abs(Vnext - V).sum() / S
    policy_delta = (policy_next != policy).sum()
    print(iteration, V[0, 0:3], Vdelta, policy_delta)
    V = Vnext
    policy = policy_next
