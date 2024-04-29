#!/usr/bin/env python3


# I think there is a problem in the action space. We are currently using a
# discrete box ranging from -127 to 127. But often, only a small subset of
# these actions is valid. E.g., for typical selfish mining in Nakamoto, there
# is at most one defender block to consider and one attacker block to release.
# So the true actions are -1, 0, 1. Currently the env silently changes the
# action to the next possible, thus the probability of randomly executing
# action 0 (continue) is only 1/255.
#
# Also, there is general advice on rl_zoo3, to use unit spaces. So, let's map
# our actions to [-1, 1]!
#
# We use the f(x) = x / (1 + |x|) relation. It yields okaish sample
# probabilities and is straightforward to compute.
#
# I'll go implement this in the environment.

import random

min_action = -127
max_action = 127


def random_action():
    return random.uniform(-1, 1)


def of_unit(x):
    if x >= 0:
        return -x / (x - 1)
    else:
        return x / (x + 1)


def discrete_of_unit(x):
    y = int(round(of_unit(x)))
    if y < min_action:
        return min_action
    if y > max_action:
        return max_action
    return y


def to_unit(x):
    return x / (1 + abs(x))


for a in [min_action, 2, -1, 0, 1, 2, max_action]:
    f = to_unit(a)
    print(a, f)
    a_ = discrete_of_unit(f)
    assert a == a_

n = 100000
float_actions = [0.0] * n
int_actions = [0] * n
weights = [0] * (max_action - min_action + 1)

for i in range(n):
    x = random_action()
    float_actions[i] = x
    x = discrete_of_unit(x)
    int_actions[i] = x
    weights[x - min_action] += 1

print(f"sampled unit: {min(float_actions)} {max(float_actions)}")
print(f"sampled discrete: {min(int_actions)} {max(int_actions)}")

for i in [min_action, min_action + 1]:
    print(f"prob {i}: {weights[i - min_action] / n}")

for i in range(-3, 4):
    print(f"prob {i}: {weights[i - min_action] / n}")

for i in [max_action - 1, max_action]:
    print(f"prob {i}: {weights[i - min_action] / n}")
