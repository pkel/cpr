import cpr_gym_rs  # noqa: 401
import gymnasium.utils.env_checker


def test_FC16SSZwPT():
    for _ in range(10):
        env = gymnasium.make("FC16SSZwPT-v0", alpha=0.5, gamma=0.5, horizon=5)
        gymnasium.utils.env_checker.check_env(env.unwrapped, skip_render_check=True)


def test_Nakamoto():
    for _ in range(10):
        env = gymnasium.make("Nakamoto-v0", alpha=0.5, gamma=0.5)
        gymnasium.utils.env_checker.check_env(env.unwrapped, skip_render_check=True)
