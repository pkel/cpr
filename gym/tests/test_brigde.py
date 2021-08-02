from cpr_gym import engine, specs


def test_engine():
    env = engine.create(specs.default)
    obs = engine.reset(env)
    obs, rew, done, info = engine.step(env, 0)
    assert not done
