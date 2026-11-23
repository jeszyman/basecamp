"""Run a study with the local model as the experimenter, on jeff-beast.

python -m local_sci.run_cli STUDY_DIR --run-dir RUN_DIR [--model M] [--rounds R] [--per-round K]

Writes the current step to RUN_DIR/status.txt; the last line reads "done: <report path>"
or "stopped: <reason>". Started by `local-sci run` on pad through systemd-run.
"""
import argparse
import datetime
import sys
from pathlib import Path

from local_sci.experimenter import run_study
from local_sci.harness import load_measure
from local_sci.llm_lane import LeaseError, NineBExperimenter, ServerLease


def main(argv=None):
    ap = argparse.ArgumentParser(prog="local-sci run")
    ap.add_argument("study")
    ap.add_argument("--run-dir", required=True)
    ap.add_argument("--model", default="qwen3.5-9b")
    ap.add_argument("--rounds", type=int, default=4)
    ap.add_argument("--per-round", type=int, default=5)
    args = ap.parse_args(argv)
    study, run = Path(args.study), Path(args.run_dir)
    run.mkdir(parents=True, exist_ok=True)
    status_file = run / "status.txt"

    def status(message):
        with open(status_file, "a") as fh:
            fh.write(f"{datetime.datetime.now():%Y-%m-%d %H:%M} {message}\n")

    status(f"started: study {study}, model {args.model}")
    mod = load_measure(str(study / "measure.py"))
    try:
        with ServerLease(model=args.model, status=status) as client:
            experimenter = NineBExperimenter(
                client, (study / "question.md").read_text(), (study / "measure.md").read_text(),
                mod.SPACE, list(getattr(mod, "CONSTRAINTS", [])), getattr(mod, "BETTER", "higher"), seed=1000,
            )
            out = run_study(study, run, experimenter, args.rounds, args.per_round, status=status)
    except LeaseError as err:
        status(f"stopped: {err}")
        return 4
    except Exception as err:
        status(f"stopped: {type(err).__name__}: {err}")
        raise
    status(f"done: {out['report']}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
