import numpy
import pickle
import sympy

protocol = "bitcoin"

with open(f"{protocol}.pkl", "rb") as f:
    transitions = pickle.load(f)

# Let's do a naive value iteration according to
# Wikipedia:Markov Decision Process

S = 0
A = 0
for act, src, dst, prob, rew in transitions:
    S = max(src, S)
    A = max(act, A)
S += 1
A += 1

print(f"{protocol} protocol, {S} states, {A} actions, {len(transitions)} transitions")

sym_params = {"α": 0.25, "γ": 0.5, "H": 1000}

# Build table t[src][act] = list[tuple[dst, prob, rew]]
tab = [dict() for _ in range(S)]
for act, src, dst, prob, rew in transitions:
    if act not in tab[src]:
        tab[src][act] = []
    if isinstance(prob, sympy.Basic):
        prob = float(prob.evalf(subs=sym_params))
    tab[src][act].append((dst, prob, rew))

value = numpy.zeros(S, dtype=float)
policy = numpy.zeros(S, dtype=int)

discount = 0.99

print("start value iteration")

for iteration in range(100):
    value_next = numpy.zeros(S, dtype=float)
    policy_next = numpy.zeros(S, dtype=int)

    for src in range(S):
        best_v = 0.0
        best_a = 0
        for act, lst in tab[src].items():
            this_v = 0.0
            for dst, prob, rew in lst:
                this_v += prob * (rew + discount * value[dst])
            if this_v > best_v:
                best_v = this_v
                best_a = act
        value_next[src] = best_v
        policy_next[src] = best_a

    value_delta = numpy.abs(value_next - value).max()
    policy_delta = (policy_next != policy).sum()
    print(iteration, value[:3], value_delta, policy_delta)
    value = value_next
    policy = policy_next
