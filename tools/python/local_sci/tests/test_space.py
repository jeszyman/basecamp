import random

import pytest

from local_sci.space import SpaceError, in_space, scripted_proposer

SPACE = {"lo": (50, 300, 5), "hi": (70, 450, 5)}
CONSTRAINTS = ["lo < hi"]


def test_in_space_accepts_a_grid_point():
    assert in_space({"lo": 100, "hi": 170}, SPACE, CONSTRAINTS)


@pytest.mark.parametrize("setting", [
    {"lo": 45, "hi": 170},            # below range
    {"lo": 102, "hi": 170},           # off the 5 bp step
    {"lo": 200, "hi": 150},           # breaks lo < hi
    {"lo": 100},                      # missing a parameter
    {"lo": 100, "hi": 170, "x": 1},   # extra parameter
    {"lo": 100.0, "hi": 170},         # not an integer
])
def test_in_space_rejects(setting):
    assert not in_space(setting, SPACE, CONSTRAINTS)


def test_bad_constraint_raises():
    with pytest.raises(SpaceError):
        in_space({"lo": 100, "hi": 170}, SPACE, ["lo << hi"])


def peak(setting):
    return -((setting["lo"] - 100) / 50) ** 2 - ((setting["hi"] - 170) / 80) ** 2


def search(n_rounds, k, seed):
    rng, state, history = random.Random(seed), {}, []
    for _ in range(n_rounds):
        for s in scripted_proposer(SPACE, CONSTRAINTS, history, k, rng, state):
            history.append((s, peak(s)))
    return history


def test_scripted_search_never_repeats_and_stays_in_space():
    history = search(4, 5, seed=1)
    keys = [(s["lo"], s["hi"]) for s, _ in history]
    assert len(keys) == len(set(keys)) == 20
    assert all(in_space({p: s[p] for p in SPACE}, SPACE, CONSTRAINTS) for s, _ in history)


def test_scripted_search_improves_on_its_random_start():
    history = search(4, 5, seed=1)
    first = max(v for _, v in history[:3])
    assert max(v for _, v in history) > first
