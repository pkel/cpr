from gymnasium.envs.registration import register

register(
    id="cpr_gym_rs/FC16SSZwPT-v0",
    entry_point="cpr_gym_rs.envs:FC16SSZwPT",
    nondeterministic=True,
    order_enforce=False,
)
