"""local-sci: answer a small research question from data on jeff-beast.

python -m local_sci.cli "<question>" --data DIR [--model M] [--out ROOT]

Copies the data folder into a new run folder, sends the question and the files'
column names and row counts to one headless Claude call, which writes measure.md
and measure.py, then runs the measure through the statistics harness and writes
results.tsv, figure.png, and report.md. The last line printed is "report: <path>".
Exit codes: 0 done, 2 planning reply unusable, 3 the no-change check failed.
"""
import argparse
import csv
import datetime
import json
import re
import shutil
import sys
from pathlib import Path

from local_sci import planner
from local_sci.harness import DuplicateSettingError, NoChangeError, load_measure, negative_control, run_sweep
from local_sci.report import plot, write_report

DEFAULT_OUT = "/mnt/data/local-sci/runs"
DEFAULT_MODEL = "claude-sonnet-5-5"


def _run_dir(root, question):
    slug = re.sub(r"[^a-z0-9]+", "-", question.lower()).strip("-")[:40].rstrip("-") or "question"
    base = Path(root) / f"{datetime.date.today():%Y-%m-%d}-{slug}"
    run, i = base, 2
    while run.exists():
        run, i = Path(f"{base}-{i}"), i + 1
    run.mkdir(parents=True)
    return run


def _tokens(reply):
    u = reply.get("usage", {}) or {}
    keys = ["input_tokens", "cache_creation_input_tokens", "cache_read_input_tokens", "output_tokens"]
    return sum(int(u.get(k, 0) or 0) for k in keys)


def _write_results(run, rows):
    fields = ["setting", "n", "n_dropped", "median", "ci_low", "ci_high", "p", "p_holm", "flagged"]
    with open(run / "results.tsv", "w", newline="") as fh:
        writer = csv.writer(fh, delimiter="\t")
        writer.writerow(fields)
        for r in rows:
            writer.writerow([r["setting"]["name"]] + ["" if r[f] is None else r[f] for f in fields[1:]])


def main(argv=None):
    ap = argparse.ArgumentParser(prog="local-sci", description=__doc__.split("\n\n")[0])
    ap.add_argument("question")
    ap.add_argument("--data", required=True, help="folder of .tsv/.csv files on jeff-beast")
    ap.add_argument("--model", default=DEFAULT_MODEL)
    ap.add_argument("--out", default=DEFAULT_OUT)
    args = ap.parse_args(argv)

    run = _run_dir(args.out, args.question)
    data = run / "data"
    data.mkdir()
    for path in Path(args.data).iterdir():
        if path.is_file():
            shutil.copy2(path, data / path.name)
    schema = f"Data folder: {data}\n\n" + planner.schema_summary(data)
    (run / "question.txt").write_text(args.question + "\n")
    (run / "schema.txt").write_text(schema)

    reply = planner.call_claude(args.question, schema, args.model)
    (run / "claude_reply.json").write_text(json.dumps(reply, indent=1))
    print(f"planning call: {_tokens(reply)} tokens ({args.model})")
    try:
        measure_md, measure_py = planner.split_reply(reply.get("result", ""))
    except planner.PlanFormatError as err:
        print(f"stopped: {err}; reply saved in {run / 'claude_reply.json'}")
        return 2
    (run / "measure.md").write_text(measure_md)
    (run / "measure.py").write_text(measure_py)
    try:
        planner.check_code(run / "measure.py")
    except planner.UnsafeCodeError as err:
        print(f"stopped: {err}")
        return 2

    mod = load_measure(str(run / "measure.py"))
    loaded, labels = mod.load(str(data))
    try:
        rows = run_sweep(mod, loaded, labels)
    except (NoChangeError, DuplicateSettingError) as err:
        print(f"stopped: {err}")
        return 3
    control = negative_control(rows, float(mod.NO_CHANGE_VALUE))
    _write_results(run, rows)
    plot(rows, float(mod.NO_CHANGE_VALUE), str(run / "figure.png"))
    report = write_report(str(run), measure_md, rows, control, "figure.png")
    print(f"report: {report}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
