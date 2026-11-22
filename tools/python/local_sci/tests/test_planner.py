from pathlib import Path

import pytest

from local_sci import planner
from local_sci.cli import main
from local_sci.planner import PlanFormatError, UnsafeCodeError, check_code, schema_summary, split_reply

HERE = Path(__file__).parent
FRAG_DIR = "/mnt/gcs/lilli/ewing/analysis_bundle/permutation_bootstrap_jeff"
MEASURE_MD = "## Quantity\nNet gain.\n\n## No-change value\n1.0"


def fixture_reply():
    source = (HERE / "fixtures" / "fragwindow_measure.py").read_text().rstrip("\n")
    return f"```markdown\n{MEASURE_MD}\n```\n\n```python\n{source}\n```"


def test_schema_summary_has_no_values(tmp_path):
    (tmp_path / "meta.tsv").write_text("sample\ttf\nES001\t0.02\nES002\t0.03\nES003\t0.01\n")
    s = schema_summary(tmp_path)
    assert "sample" in s and "ES001" not in s and "rows: 3" in s


def test_split_reply_two_blocks():
    md, py = split_reply(fixture_reply())
    assert md.startswith("## Quantity") and "def measure" in py


def test_split_reply_rejects_missing_block():
    with pytest.raises(PlanFormatError):
        split_reply("```markdown\nx\n```")


def test_split_reply_rejects_prose_around_blocks():
    with pytest.raises(PlanFormatError):
        split_reply("Here is the plan.\n" + fixture_reply())


def test_check_code_refuses_subprocess(tmp_path):
    p = tmp_path / "measure.py"
    p.write_text("import subprocess\n")
    with pytest.raises(UnsafeCodeError):
        check_code(p)


def test_check_code_refuses_writing_a_file(tmp_path):
    p = tmp_path / "measure.py"
    p.write_text("def load(d):\n    open(d + '/x.tsv', 'w').write('y')\n")
    with pytest.raises(UnsafeCodeError):
        check_code(p)


def test_check_code_accepts_the_fixture():
    check_code(HERE / "fixtures" / "fragwindow_measure.py")


@pytest.mark.skipif(not Path(FRAG_DIR).exists(), reason="Ewing data only on beast")
def test_cli_end_to_end_with_stubbed_call(tmp_path, monkeypatch):
    monkeypatch.setattr(planner, "call_claude", lambda q, s, m: {"result": fixture_reply(), "usage": {"output_tokens": 1}})
    assert main(["q", "--data", FRAG_DIR, "--out", str(tmp_path)]) == 0
    run = next(tmp_path.iterdir())
    report = (run / "report.md").read_text()
    assert report.startswith(MEASURE_MD)
    assert (run / "figure.png").exists() and (run / "data" / "sample_selection_metadata.tsv").exists()
    assert "55-170 | 28 | 0 | 2.409" in report
