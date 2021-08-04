import gym
from cpr_gym import specs


def test_default(capsys):
    env = gym.make('cpr-v0')
    env.render()
    captured = capsys.readouterr().out.splitlines()[0]
    assert captured == 'Bₖ with k=51 and α=0.25'


def test_spec_arg(capsys):
    env = gym.make('cpr-v0', spec=specs.bk(k=8, alpha=.33))
    env.render()
    captured = capsys.readouterr().out.splitlines()[0]
    assert captured == 'Bₖ with k=8 and α=0.33'


def test_policies():
    env = gym.make('cpr-v0', spec=specs.bk(k=8, alpha=.33))
    p = env.policies()['selfish']
    obs = env.reset()
    for x in range(100):
        obs, _, _, _ = env.step(p(obs))
