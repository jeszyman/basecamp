import numpy as np
import pytest

from local_sci import bench_tissue as bt
from local_sci.harness import load_measure


def test_targets_hold_each_cell_type_and_family_once():
    names = [bt.TARGETS[i][0] for i in range(1, 46)]
    assert sorted(names) == sorted(bt.CELL_TYPES + list(bt.FAMILIES))
    assert bt.TARGETS[0][1] == bt.CELL_TYPES
    assert sum(len(m) for m in bt.FAMILIES.values()) == 36
    assert sorted(c for m in bt.FAMILIES.values() for c in m) == sorted(bt.CELL_TYPES)
    assert len(bt.PLANTABLE) == 29


def test_planting_moves_the_unmethylated_share_toward_the_atlas():
    counts = np.array([[10.0, 30.0, 60.0], [0.0, 0.0, 0.0], [50.0, 0.0, 50.0]])
    ref = np.array([0.9, 0.9, np.nan])
    out = bt.plant_counts(counts, ref, 0.1)
    assert out[0, 0] / out[0].sum() == pytest.approx(0.9 * 0.1 + 0.1 * 0.9)
    assert out[0].sum() == pytest.approx(100.0)
    assert out[0, 1] / (out[0, 1] + out[0, 2]) == pytest.approx(30 / 90)
    assert (out[1] == 0).all() and (out[2] == counts[2]).all()


def _study(tmp_path, planted_excess):
    """A study folder with fractions written directly: cases carry planted_excess of Kidney-Ep."""
    study = tmp_path / "s"
    (study / "data").mkdir(parents=True)
    base = {c: 1 / 36 for c in bt.CELL_TYPES}
    rows = []
    for i in range(55):
        fr = dict(base)
        label = "case" if i < 28 else "ref"
        fr["Blood-T"] += 0.001 * (i % 5)
        if label == "case":
            fr["Kidney-Ep"] += planted_excess
        rows.append((f"s{i}", label, fr))
    with open(study / "data" / "fractions.tsv", "w") as fh:
        fh.write("sample\tlabel\t" + "\t".join(bt.CELL_TYPES) + "\n")
        for s, label, fr in rows:
            fh.write(f"{s}\t{label}\t" + "\t".join(str(fr[c]) for c in bt.CELL_TYPES) + "\n")
    (study / "measure.py").write_text(bt.MEASURE.format(members={i: m for i, (_, m) in bt.TARGETS.items()}))
    return study


def test_family_scores_below_its_planted_member_and_target_0_is_zero(tmp_path):
    study = _study(tmp_path, 0.02)
    mod = load_measure(str(study / "measure.py"))
    data, labels = mod.load(str(study / "data"))
    member = mod.measure(data, labels, {"target": bt.INDEX["Kidney-Ep"]})
    family = mod.measure(data, labels, {"target": bt.INDEX["Other epithelium family"]})
    assert all(member[k] == pytest.approx(2.0) for k in member)
    assert all(family[k] == pytest.approx(2.0 / 9) for k in family)
    assert all(v == 0.0 for v in mod.measure(data, labels, {"target": 0}).values())
    assert bt.exhaustive_pick(study) == bt.INDEX["Kidney-Ep"]


def test_family_first_search_tries_families_then_members_of_the_best():
    search = bt.FamilyFirstSearch()
    first, _ = search.propose([], 5, 20)
    assert [p["target"] for p in first] == search.families[:5]
    history = [{"setting": {"target": t, "name": str(t)},
                "row": {"median": 1.0 if t == bt.INDEX["Lung family"] else 0.0}} for t in search.families]
    nxt, _ = search.propose(history, 5, 11)
    assert [p["target"] for p in nxt[:2]] == [bt.INDEX["Lung-Ep-Alveo"], bt.INDEX["Lung-Ep-Bron"]]


def test_sign_test_counts_studies_each_way():
    assert bt.sign_test([2, 2, 1, 0], [0, 1, 1, 0])[:2] == (2, 0)
    assert bt.sign_test([1] * 10, [0] * 10)[2] == pytest.approx(2 / 2 ** 10)
    assert bt.sign_test([0, 0], [0, 0]) == (0, 0, 1.0)
