import gymnasium.utils.env_checker


def test_FC16SSZwPT():
    for _ in range(10):
        env = gymnasium.make(
            "cpr_gym_rs/FC16SSZwPT-v0", alpha=0.25, gamma=0.25, horizon=5
        )
        gymnasium.utils.env_checker.check_env(env.unwrapped, skip_render_check=True)
