from cpr_gym import engine, protocols


def test_engine():
    env = engine.create(
        proto=protocols.nakamoto(unit_observation=False),
        alpha=0.33,
        gamma=0.5,
        defenders=2,
        activation_delay=1,
    )
    obs = engine.reset(env)
    obs, rew, done, info = engine.step(env, 0)
    assert not done


def test_engine_600steps():
    # see if the engine works for 600 steps.
    # We had some memory management (gc?) problems in the beginning.
    env = engine.create(
        proto=protocols.nakamoto(unit_observation=True),
        alpha=0.33,
        gamma=0.5,
        defenders=2,
        activation_delay=1,
    )
    obs = engine.reset(env)
    for i in range(600):
        obs, rew, done, info = engine.step(env, 3)
    assert not done
