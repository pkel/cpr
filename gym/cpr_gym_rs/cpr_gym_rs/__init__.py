from gymnasium.envs.registration import register

register(
    id="FC16SSZwPT-v0",
    entry_point="cpr_gym_rs.envs:FC16SSZwPT",
    nondeterministic=True,
    order_enforce=False,
    max_episode_steps=1000,  # forced termination, not Markovian
    kwargs=dict(
        alpha=1 / 3,
        gamma=0.5,
        horizon=100,
    ),
)
