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
    """A measure module whose value per sample is fixed per setting name."""
    settings = settings or [{"name": n} for n in values_by_setting]
    return SimpleNamespace(
        CASE_LABEL="a",
        NO_CHANGE_VALUE=null,
        SETTINGS=settings,
        load=lambda d: ({}, dict(LABELS)),
        measure=lambda data, labels, setting: {
            s: v for s, v in zip(sorted(x for x, l in labels.items() if l == "a"), values_by_setting[setting["name"]])
        },
    )


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
    assert (rows["55-170"]["ci_low"], rows["55-170"]["ci_high"]) == pytest.approx((-2.7795, 3.6726), abs=1e-4)
    assert not any(r["flagged"] for r in rows.values())
