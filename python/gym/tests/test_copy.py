import copy
import gym
import numpy


def test_copy():
    for x in range(30):
        a = gym.make("cpr_gym:core-v0")
        obsa = a.reset()
        for x in range(30):
            obsa, _, _, _ = a.step(a.policy(obsa, "honest"))
        b = copy.deepcopy(a)
        obsb = obsa
        for x in range(30):
            obsa, _, _, _ = a.step(a.policy(obsa, "honest"))
            obsb, _, _, _ = b.step(b.policy(obsb, "honest"))
            assert numpy.array_equal(obsa, obsb)
