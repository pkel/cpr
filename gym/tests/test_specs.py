import gym
from cpr_gym import specs


def test_default(capsys):
    env = gym.make('cpr-v0')
    env.render()
    captured = capsys.readouterr()
    assert captured.out == 'Bₖ with k=51 and α=0.25\n'


def test_spec_arg(capsys):
    env = gym.make('cpr-v0', spec=specs.bk(k=8, alpha=.33))
    env.render()
    captured = capsys.readouterr()
    assert captured.out == 'Bₖ with k=8 and α=0.33\n'
