import gym
import cpr_gym  # noqa


def run_episode(env, policy):
    obs = env.reset()
    done = False
    while not done:
        obs, rew, done, info = env.step(env.policy(obs, policy))
    return obs, rew, done, info


def run_honest_once(env_name):
    env = gym.make(env_name, episode_len=1024)
    run_episode(env, "honest")


def test_nakamoto_v0(benchmark):
    benchmark(run_honest_once, "cpr-nakamoto-v0")


def test_tailstorm_v0(benchmark):
    benchmark(run_honest_once, "cpr-tailstorm-v0")
