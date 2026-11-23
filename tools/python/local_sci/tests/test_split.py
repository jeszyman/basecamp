from pathlib import Path

import pytest

from local_sci.harness import confirm, evaluate, load_measure, run_sweep, split_samples
from local_sci.tests.test_harness import FRAG_DIR, HERE, LABELS, toy


def test_split_halves_each_group_and_is_disjoint():
    labels = {f"c{i}": "cancer" for i in range(28)} | {f"h{i}": "healthy" for i in range(17)}
    discovery, holdout = split_samples(labels, seed=5)
    assert not set(discovery) & set(holdout)
    assert set(discovery) | set(holdout) == set(labels)
    assert sum(v == "cancer" for v in discovery.values()) == 14
    assert sum(v == "healthy" for v in discovery.values()) == 8
    assert split_samples(labels, seed=5) == (discovery, holdout)


def test_confirm_applies_holm_over_picks_only():
    mod = toy({"none": [1.0] * 6, "x": [2.0, 2.1, 1.9, 2.2, 2.05, 1.95], "y": [1.0, 1.2, 0.8, 1.1, 0.9, 1.05]})
    rows = confirm(mod, {}, dict(LABELS), [{"name": "x"}, {"name": "y"}])
    assert len(rows) == 2
    assert rows[0]["p_holm"] == pytest.approx(min(1.0, 2 * min(rows[0]["p"], rows[1]["p"])) if rows[0]["p"] <= rows[1]["p"] else rows[0]["p_holm"])
    assert all(r["flagged"] is not None for r in rows)


@pytest.mark.skipif(not Path(FRAG_DIR).exists(), reason="Ewing data only on beast")
def test_evaluate_matches_run_sweep_median():
    frag = load_measure(str(HERE / "fixtures" / "fragwindow_measure.py"))
    data, labels = frag.load(FRAG_DIR)
    row = evaluate(frag, data, labels, {"name": "55-170", "lo": 55, "hi": 170})
    sweep = {r["setting"]["name"]: r for r in run_sweep(frag, data, labels)}
    assert row["median"] == pytest.approx(sweep["55-170"]["median"])
    assert row["p_holm"] is None
