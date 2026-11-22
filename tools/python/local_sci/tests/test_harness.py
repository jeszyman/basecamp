import random
import statistics
from pathlib import Path
from types import SimpleNamespace

import pytest

from local_sci.harness import (
    DuplicateSettingError,
    NoChangeError,
    holm,
    load_measure,
    run_sweep,
)

HERE = Path(__file__).parent
FRAG_DIR = "/mnt/gcs/lilli/ewing/analysis_bundle/permutation_bootstrap_jeff"
SAMPLES = [f"s{i}" for i in range(6)]
LABELS = {s: ("a" if i < 6 else "b") for i, s in enumerate(SAMPLES)} | {"r0": "b", "r1": "b"}


def toy(values_by_setting, settings=None, null=1.0):
    """A measure module whose value for case sample s{i} is the i-th value of its setting."""
    settings = settings or [{"name": n} for n in values_by_setting]

    def measure(data, labels, setting):
        vals = values_by_setting[setting["name"]]
        return {s: vals[int(s.split("#")[0][1:])] for s, label in labels.items() if label == "a"}

    return SimpleNamespace(CASE_LABEL="a", NO_CHANGE_VALUE=null, SETTINGS=settings,
                           load=lambda d: ({}, dict(LABELS)), measure=measure)


def test_holm_known_values():
    assert holm([0.01, 0.04, 0.03]) == pytest.approx([0.03, 0.06, 0.06])


def test_no_change_gate_raises():
    mod = toy({"none": [1.1] * 6, "x": [2.0] * 6})
    with pytest.raises(NoChangeError):
        run_sweep(mod, {}, dict(LABELS))


def test_duplicate_settings_refused():
    mod = toy({"none": [1.0] * 6, "x": [2.0] * 6}, settings=[{"name": "none"}, {"name": "x"}, {"name": "x"}])
    with pytest.raises(DuplicateSettingError):
        run_sweep(mod, {}, dict(LABELS))


def test_nonfinite_values_dropped_and_counted():
    inf = float("inf")
    mod = toy({"none": [1.0] * 6, "x": [2.0, 2.1, 1.9, 2.2, inf, float("nan")]})
    row = run_sweep(mod, {}, dict(LABELS))[1]
    assert row["n"] == 4 and row["n_dropped"] == 2


def test_fewer_than_five_samples_no_interval():
    mod = toy({"none": [1.0] * 6, "x": [2.0, 2.1, float("nan"), float("nan"), float("nan"), 2.2]})
    row = run_sweep(mod, {}, dict(LABELS))[1]
    assert row["ci_low"] is None and row["flagged"] is None


@pytest.mark.skipif(not Path(FRAG_DIR).exists(), reason="Ewing data only on beast")
def test_reproduces_fragment_window():
    frag = load_measure(str(HERE / "fixtures" / "fragwindow_measure.py"))
    rows = {r["setting"]["name"]: r for r in run_sweep(frag, *frag.load(FRAG_DIR))}
    assert rows["55-170"]["median"] == pytest.approx(2.4092, abs=1e-4)
    assert rows["55-130"]["median"] == pytest.approx(0.6224, abs=1e-4)
    assert rows["55-170"]["ci_low"] < 1 < rows["55-170"]["ci_high"]
    assert not any(r["flagged"] for r in rows.values())


def test_shared_reference_widens_interval():
    """A measure that divides by the mean of the other group must carry that mean's error."""
    labels = {f"s{i}": "a" for i in range(20)} | {f"r{i}": "b" for i in range(20)}
    rng = random.Random(3)
    case = [rng.gauss(2.0, 0.05) for _ in range(20)]
    ref = [rng.gauss(1.0, 0.5) for _ in range(20)]

    def measure(data, lab, setting):
        idx = lambda s: int(s.split("#")[0][1:])  # noqa: E731
        refs = [ref[idx(s)] for s, l in lab.items() if l == "b"]
        scale = statistics.mean(refs) if setting["name"] == "x" else 1.0
        return {s: case[idx(s)] / scale if setting["name"] == "x" else 1.0 for s, l in lab.items() if l == "a"}

    mod = SimpleNamespace(CASE_LABEL="a", NO_CHANGE_VALUE=1.0, SETTINGS=[{"name": "none"}, {"name": "x"}],
                          load=None, measure=measure)
    row = run_sweep(mod, {}, labels)[1]
    assert row["ci_high"] - row["ci_low"] > 0.3
