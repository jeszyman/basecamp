import importlib.util
import os

import numpy as np
import pytest

HERE = os.path.dirname(__file__)
PATH = os.path.join(HERE, "..", "studies", "uxm-settings", "measure.py")


@pytest.fixture
def mod():
    spec = importlib.util.spec_from_file_location("uxm_measure", PATH)
    m = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(m)
    return m


def _data(mod, mixtures, per_type=30, reads=100000, seed=3):
    """Synthetic atlas of per_type markers per cell type; each sample's U counts are
    its mixture times the atlas, so deconvolution should return the mixture."""
    rng = np.random.default_rng(seed)
    n_types = len(mod.CELL_TYPES)
    target = np.repeat(mod.CELL_TYPES, per_type)
    rank = np.tile(np.arange(per_type), n_types)
    ref = np.full((len(target), n_types), 0.05) + rng.uniform(0, 0.05, (len(target), n_types))
    for i, t in enumerate(target):
        ref[i, mod.CELL_TYPES.index(t)] = 0.9
    counts = {}
    for sample, mix in mixtures.items():
        u = ref @ mix
        for rlen in mod.RLENS:
            counts[(sample, rlen)] = np.column_stack([u * reads, (1 - u) * reads, np.zeros(len(u))])
    return {"target": target, "rank": rank, "ref": ref, "counts": counts}


def _mix(mod, weights):
    v = np.zeros(len(mod.CELL_TYPES))
    for t, w in weights.items():
        v[mod.CELL_TYPES.index(t)] = w
    return v / v.sum()


def test_deconvolution_recovers_a_known_epithelial_fraction(mod):
    data = _data(mod, {"p1": _mix(mod, {"Blood-Granul": 0.6, "Blood-T": 0.2, "Breast-Luminal-Ep": 0.15, "Liver-Hep": 0.05})})
    assert mod.epithelial_fraction(data, "p1", 25, 4, 1) == pytest.approx(0.15, abs=1e-6)


def test_no_change_setting_gives_zero_for_every_patient(mod):
    mixes = {f"h{i}": _mix(mod, {"Blood-Granul": 0.7 + 0.01 * i, "Blood-T": 0.3}) for i in range(4)}
    mixes.update({f"c{i}": _mix(mod, {"Blood-Granul": 0.6, "Breast-Luminal-Ep": 0.05 * (i + 1)}) for i in range(3)})
    data = _data(mod, mixes)
    labels = {s: ("healthy" if s.startswith("h") else "cancer") for s in mixes}
    values = mod.measure(data, labels, mod.NO_CHANGE)
    assert set(values) == {"c0", "c1", "c2"}
    assert all(v == 0.0 for v in values.values())


def test_markers_setting_keeps_only_the_top_ranked_markers(mod):
    data = _data(mod, {"p1": _mix(mod, {"Blood-Granul": 1.0})})
    rows = np.isin(data["target"], mod.CELL_TYPES) & (data["rank"] < 10)
    assert rows.sum() == 10 * len(mod.CELL_TYPES)
    assert mod.epithelial_fraction(data, "p1", 10, 4, 1) == pytest.approx(0.0, abs=1e-6)


def test_dropping_breast_types_leaves_them_out_of_the_mixture(mod):
    data = _data(mod, {"p1": _mix(mod, {"Blood-Granul": 0.8, "Breast-Luminal-Ep": 0.2})})
    with_breast = mod.epithelial_fraction(data, "p1", 25, 4, 1)
    without = mod.epithelial_fraction(data, "p1", 25, 4, 0)
    assert with_breast == pytest.approx(0.2, abs=1e-6)
    assert without != pytest.approx(0.2, abs=1e-3)
