from gymnasium.envs.registration import register

register(
    id="FC16SSZwPT-v0",
    entry_point="cpr_gym_rs.envs:FC16SSZwPT",
    nondeterministic=True,
    order_enforce=False,
    kwargs=dict(
        alpha=0.4,
        gamma=0.0,
        horizon=25,
    ),
)
