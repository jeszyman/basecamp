import pytest

from local_sci import bench_real
from local_sci.bench import SPACE


def _freqs(peak=167):
    raw = [1.0 / (1 + ((n - peak) / 15.0) ** 2) for n in bench_real.LENGTHS]
    total = sum(raw)
    return [r / total for r in raw]


def test_plants_lie_on_the_grid_inside_the_space():
    for lo, hi in bench_real.PLANTED.values():
        assert 100 <= lo <= 200 and lo + 30 <= hi <= 260
        assert lo % 5 == 0 and hi % 5 == 0
        assert SPACE["lo"][0] <= lo <= SPACE["lo"][1] and SPACE["hi"][0] <= hi <= SPACE["hi"][1]
    assert len(bench_real.PLANTED) == 6 and bench_real.draw_plants() == bench_real.PLANTED


def test_planting_moves_the_stated_share_into_the_window():
    f = _freqs()
    g = bench_real.plant(f, (140, 190), 0.2)
    inside = lambda x: sum(v for n, v in zip(bench_real.LENGTHS, x) if 140 <= n <= 190)
    assert sum(g) == pytest.approx(1.0)
    assert inside(g) == pytest.approx(0.8 * inside(f) + 0.2)


def test_assignment_is_fixed_by_the_seed_and_has_28_cases():
    samples = {f"s{i}": None for i in range(45)}
    a = bench_real.assign(samples, 3)
    assert a == bench_real.assign(samples, 3)
    assert sum(v == "case" for v in a.values()) == 28


def test_known_best_is_the_planted_window_when_samples_share_one_shape(tmp_path):
    source = {f"s{i}": _freqs() for i in range(45)}
    study = bench_real.make_study(tmp_path, "toy", (150, 185), source, seed=1)
    best, value = bench_real.grid_best(study)
    assert (best["lo"], best["hi"]) == (150, 185)
    assert value > 0


def test_score_hits_at_the_bar_and_misses_below_it(tmp_path):
    source = {f"s{i}": _freqs() for i in range(45)}
    study = bench_real.make_study(tmp_path, "toy", (150, 185), source, seed=1)
    best, best_value = bench_real.grid_best(study)
    bar = bench_real.value_at(study, best)
    assert bench_real.score(study, dict(best), bar, best_value) == (True, pytest.approx(1.0))
    worse_hit, worse_share = bench_real.score(study, {"lo": 100, "hi": 300}, bar, best_value)
    assert not worse_hit and worse_share < 1.0
    assert bench_real.score(study, None, bar, best_value) == (False, 0.0)


def test_measure_is_zero_at_the_no_change_setting(tmp_path):
    source = {f"s{i}": _freqs(160 + i % 5) for i in range(45)}
    study = bench_real.make_study(tmp_path, "toy", (150, 185), source, seed=2)
    from local_sci.harness import load_measure
    mod = load_measure(study / "measure.py")
    data, labels = mod.load(str(study / "data"))
    values = mod.measure(data, labels, mod.NO_CHANGE)
    assert len(values) == 28 and all(v == 0.0 for v in values.values())
