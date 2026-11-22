import random
from pathlib import Path

from local_sci.harness import negative_control, run_sweep
from local_sci.report import plot, write_report
from local_sci.tests.test_harness import LABELS, toy

MEASURE_MD = "## Quantity\nA test quantity.\n\n## No-change value\n1.0\n"
ROWS = [
    {"setting": {"name": "none"}, "n": 6, "n_dropped": 0, "median": 1.0, "ci_low": 1.0, "ci_high": 1.0,
     "p": None, "p_holm": None, "flagged": None},
    {"setting": {"name": "x"}, "n": 6, "n_dropped": 0, "median": 2.0, "ci_low": 1.5, "ci_high": 2.5,
     "p": 0.001, "p_holm": 0.002, "flagged": True},
    {"setting": {"name": "y"}, "n": 6, "n_dropped": 0, "median": 1.1, "ci_low": 0.6, "ci_high": 1.6,
     "p": 0.4, "p_holm": 0.4, "flagged": False},
]
CONTROL = {"n_rep": 20, "n_tested": 40, "n_flagged": 2, "rate": 0.05}


def test_report_opens_with_measure_md_verbatim(tmp_path):
    text = Path(write_report(str(tmp_path), MEASURE_MD, ROWS, CONTROL, "figure.png")).read_text()
    assert text.startswith(MEASURE_MD)


def test_report_has_one_row_per_setting(tmp_path):
    text = Path(write_report(str(tmp_path), MEASURE_MD, ROWS, CONTROL, "figure.png")).read_text()
    table = [line for line in text.splitlines() if line.startswith("| ") and not line.startswith("| Setting")]
    assert len(table) == len(ROWS)
    assert "0.0020" in text and "2 of 40" in text


def test_negative_control_counts():
    mod = toy({"none": [1.0] * 6, "x": [2.0, 2.1, 1.9, 2.2, 2.05, 1.95], "y": [1.0, 1.2, 0.8, 1.1, 0.9, 1.05]})
    control = negative_control(run_sweep(mod, {}, dict(LABELS)), 1.0, n_rep=5)
    assert control["n_tested"] == 5 * 2
    assert 0 <= control["n_flagged"] <= control["n_tested"]


def test_negative_control_rate_low_on_shifted_data():
    rng = random.Random(7)
    vals = [rng.gauss(3.0, 1.0) for _ in range(30)]
    mod = toy({"none": [1.0] * 30, "x": vals}, null=1.0)
    rows = run_sweep(mod, {}, {f"s{i}": "a" for i in range(30)})
    assert rows[1]["flagged"] is True
    control = negative_control(rows, 1.0, n_rep=40)
    assert control["rate"] <= 0.15


def test_plot_writes_png(tmp_path):
    path = tmp_path / "figure.png"
    plot(ROWS, 1.0, str(path))
    assert path.exists() and path.stat().st_size > 0
