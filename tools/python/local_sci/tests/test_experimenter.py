from pathlib import Path

import pytest

from local_sci.experimenter import ScriptedSearch, run_study
from local_sci.report import write_study_report
from local_sci.tests.test_harness import FRAG_DIR, HERE


def make_study(tmp_path):
    study = tmp_path / "study"
    study.mkdir()
    (study / "question.md").write_text("Which fragment-length window gives the largest net gain?\n")
    (study / "measure.md").write_text("## Quantity\nNet gain per cancer sample.\n")
    (study / "measure.py").write_text((HERE / "fixtures" / "fragwindow_measure.py").read_text())
    (study / "data.txt").write_text(FRAG_DIR + "\n")
    return study


class EmptyExperimenter(ScriptedSearch):
    def propose(self, history, k, budget_left):
        return [{"lo": 1, "hi": 2, "reason": "outside"}], {"raw": "outside"}


@pytest.mark.skipif(not Path(FRAG_DIR).exists(), reason="Ewing data only on beast")
def test_scripted_study_runs_and_reports(tmp_path):
    study = make_study(tmp_path)
    exp = ScriptedSearch({"lo": (50, 300, 5), "hi": (70, 450, 5)}, ["lo < hi"], "higher", seed=1)
    out = run_study(study, tmp_path / "run", exp, rounds=2, per_round=3)
    run = tmp_path / "run"
    assert len(out["history"]) == 6
    assert 1 <= len(out["confirm"]) <= 3
    for name in ("report.md", "experiments.tsv", "proposals.jsonl", "figure.png"):
        assert (run / name).exists()
    report = (run / "report.md").read_text()
    assert "## Findings" in report and "held-out half (14 case samples)" in report


@pytest.mark.skipif(not Path(FRAG_DIR).exists(), reason="Ewing data only on beast")
def test_two_empty_rounds_end_the_search(tmp_path):
    study = make_study(tmp_path)
    exp = EmptyExperimenter({"lo": (50, 300, 5), "hi": (70, 450, 5)}, ["lo < hi"], "higher", seed=1)
    out = run_study(study, tmp_path / "run", exp, rounds=4, per_round=3)
    assert out["history"] == []
    lines = (tmp_path / "run" / "proposals.jsonl").read_text().splitlines()
    assert sum('"round": 3' in l for l in lines) == 0


def test_report_withholds_an_unsupported_claim(tmp_path):
    row = {"setting": {"name": "x"}, "n": 14, "median": 1.2, "ci_low": 0.5, "ci_high": 2.0,
           "p_holm": 0.4, "flagged": False}
    path = write_study_report(str(tmp_path), "q", "m", [], [row], {"rate": None}, "Window x is significantly better.", "f.png", 14)
    text = Path(path).read_text()
    assert "No setting was confirmed" in text and "withheld" in text and "significantly" not in text
