"""local-sci plan: draft a study with one Claude call, for the local model to run later.

python -m local_sci.cli plan "<question>" --data DIR --study STUDY_DIR [--model M]

Sends the question and the data files' column names and row counts (no values) to
one headless Claude call, which writes measure.md and measure.py. Writes the study
folder (question.md, measure.md, measure.py, data.txt), checks the code, loads the
data, and checks that the no-change setting returns the no-change value, so the
study is ready for `local-sci run`. Read measure.md before running it.
Exit codes: 0 ready, 2 planning reply or code unusable, 3 the no-change check failed.
"""
import argparse
import json
import sys
from pathlib import Path

from local_sci import planner
from local_sci.harness import NoChangeError, check_no_change, load_measure

DEFAULT_MODEL = "claude-sonnet-5-5"
REQUIRED = ("CASE_LABEL", "NO_CHANGE_VALUE", "NO_CHANGE", "SPACE", "load", "measure")


def _tokens(reply):
    u = reply.get("usage", {}) or {}
    keys = ["input_tokens", "cache_creation_input_tokens", "cache_read_input_tokens", "output_tokens"]
    return sum(int(u.get(k, 0) or 0) for k in keys)


def main(argv=None):
    ap = argparse.ArgumentParser(prog="local-sci", description=__doc__.split("\n\n")[0])
    sub = ap.add_subparsers(dest="command", required=True)
    plan = sub.add_parser("plan", help="draft a study with one Claude call")
    plan.add_argument("question")
    plan.add_argument("--data", required=True, help="folder of .tsv/.csv files on jeff-beast")
    plan.add_argument("--study", required=True, help="new folder for the study")
    plan.add_argument("--model", default=DEFAULT_MODEL)
    args = ap.parse_args(argv)

    study = Path(args.study)
    study.mkdir(parents=True, exist_ok=False)
    schema = f"Data folder: {args.data}\n\n" + planner.schema_summary(args.data)
    (study / "question.md").write_text(args.question.strip() + "\n")
    (study / "data.txt").write_text(str(args.data) + "\n")

    reply = planner.call_claude(args.question, schema, args.model)
    (study / "claude_reply.json").write_text(json.dumps(reply, indent=1))
    print(f"planning call: {_tokens(reply)} tokens ({args.model})")
    try:
        measure_md, measure_py = planner.split_reply(reply.get("result", ""))
    except planner.PlanFormatError as err:
        print(f"stopped: {err}; reply saved in {study / 'claude_reply.json'}")
        return 2
    (study / "measure.md").write_text(measure_md)
    (study / "measure.py").write_text(measure_py)
    try:
        planner.check_code(study / "measure.py")
    except planner.UnsafeCodeError as err:
        print(f"stopped: {err}")
        return 2
    mod = load_measure(str(study / "measure.py"))
    missing = [name for name in REQUIRED if not hasattr(mod, name)]
    if missing:
        print(f"stopped: measure.py does not define {', '.join(missing)}")
        return 2
    data, labels = mod.load(str(args.data))
    try:
        check_no_change(mod, data, labels, mod.NO_CHANGE)
    except NoChangeError as err:
        print(f"stopped: {err}")
        return 3
    print(f"study ready: {study} (read measure.md, then: local-sci run {study})")
    return 0


if __name__ == "__main__":
    sys.exit(main())
