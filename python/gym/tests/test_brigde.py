from cpr_gym import engine, protocols


def test_engine():
    env = engine.create(proto=protocols.nakamoto())
    obs = engine.reset(env)
    obs, rew, done, info = engine.step(env, 0)
    assert not done


def test_engine_600steps():  # see if the engine works for 600 steps, the policy failed.
    env = engine.create(proto=protocols.nakamoto())
    obs = engine.reset(env)
    for i in range(600):
        obs, rew, done, info = engine.step(env, 3)
    assert not done
