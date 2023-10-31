import gzip
import pickle

with gzip.open("explored-models/models.pkl.gz", "rb") as f:
    models = pickle.load(f)


def load_mdp(key):
    with gzip.open(f"explored-models/{key}.pkl.gz", "rb") as f:
        data = pickle.load(f)
    return data["mdp"]


def check_redundancy(mdp):
    # we are looking for state/actions with the same set of transitions
    # and also for states with the same set of actions/transitions
    action_dicts = set()
    transition_lists = set()
    n_state_actions = 0
    for state, actions in enumerate(mdp.tab):
        hashable_actions = []
        for act, transitions in actions.items():
            n_state_actions += 1
            # sorted & hashable transition list
            tl = tuple(sorted(transitions))
            transition_lists.add(tl)

            hashable_actions.append((act, tl))
        # sorted & hashable action dict
        ad = tuple(sorted(hashable_actions))
        action_dicts.add(ad)

    tl_redundancy = 1 - (len(transition_lists) / n_state_actions)
    state_redundancy = 1 - (len(action_dicts) / mdp.n_states)

    return tl_redundancy, state_redundancy


for key in models.key:
    mdp = load_mdp(key)
    tl, state = check_redundancy(mdp)
    print(
        f"{key}:  {tl*100:.0f}% tx list redundancy / {state*100:.0f}% state redundancy"
    )
