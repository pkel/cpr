from cpr_gym import engine, environments


def test_engine():
    env = engine.create(environments.test)
    obs = engine.reset(env)
    obs, rew, done = engine.step(env, 0)
    assert not done
