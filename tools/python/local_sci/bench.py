"""Test bench: does the 9B find a planted best setting, compared with a scripted search?

Three synthetic studies plant a best setting in the fragment-window space
(lo 50-300, hi 70-450, steps of 5, lo < hi): the measure is 1 plus a smooth peak
of height 1 centred on the planted setting, plus a fixed per-sample offset
(standard deviation 0.3), for 30 case samples. A fourth study has no effect
anywhere (1 plus the per-sample offset at every setting). The 9B and the
scripted search each run 10 times per planted study at the default budget
(4 rounds of 5); a run hits when its first pick is within one step (5) of the
planted setting on both parameters. The scripted search runs the null study 20
times, and a run counts as a false flag when any pick is confirmed.

    python -m local_sci.bench OUT_DIR [--skip-9b]
"""
import argparse
import random
import sys
from pathlib import Path

from local_sci.experimenter import ScriptedSearch, run_study

SPACE = {"lo": (50, 300, 5), "hi": (70, 450, 5)}
CONSTRAINTS = ["lo < hi"]
PLANTED = {"near-short": (100, 170), "long": (200, 350), "narrow-short": (70, 120)}
N_CASES, N_REFS = 30, 20

MEASURE_TEMPLATE = '''"""Synthetic measure for the local-sci bench (planted best {planted})."""
import csv
import math
import os

CASE_LABEL = "case"
NO_CHANGE_VALUE = 1.0
NO_CHANGE = {{"name": "50-450", "lo": 50, "hi": 450}}
SPACE = {{"lo": (50, 300, 5), "hi": (70, 450, 5)}}
CONSTRAINTS = ["lo < hi"]
BETTER = "higher"
PEAK = {peak}


def load(data_dir):
    with open(os.path.join(data_dir, "samples.tsv"), newline="") as fh:
        rows = list(csv.DictReader(fh, delimiter="\\t"))
    return {{r["sample"]: float(r["offset"]) for r in rows}}, {{r["sample"]: r["label"] for r in rows}}


def measure(data, labels, setting):
    out = {{}}
    for key, label in labels.items():
        if label != CASE_LABEL:
            continue
        if setting["lo"] == 50 and setting["hi"] == 450:
            out[key] = 1.0
            continue
        bump = 0.0
        if PEAK is not None:
            bump = math.exp(-((setting["lo"] - PEAK[0]) / 40.0) ** 2 - ((setting["hi"] - PEAK[1]) / 60.0) ** 2)
        out[key] = 1.0 + bump + data[key.split("#")[0]]
    return out
'''


def make_study(root, name, peak, seed):
    study = root / "studies" / name
    data = study / "data"
    data.mkdir(parents=True, exist_ok=True)
    rng = random.Random(seed)
    lines = ["sample\tlabel\toffset"]
    lines += [f"c{i}\tcase\t{rng.gauss(0, 0.3):.6f}" for i in range(N_CASES)]
    lines += [f"r{i}\tref\t0.0" for i in range(N_REFS)]
    (data / "samples.tsv").write_text("\n".join(lines) + "\n")
    planted = "none" if peak is None else f"{peak[0]}-{peak[1]}"
    (study / "measure.py").write_text(MEASURE_TEMPLATE.format(planted=planted, peak=repr(peak)))
    (study / "question.md").write_text("Which fragment-length window (lo, hi) gives the highest value of the measure?\n")
    (study / "measure.md").write_text(
        "## Quantity\nA per-sample score for a window [lo, hi] bp; 1 means no change and higher is better.\n\n"
        "## No-change value\n1.0, at the full range 50-450.\n")
    (study / "data.txt").write_text(str(data) + "\n")
    return study


def hit(first_pick, peak):
    return abs(first_pick["lo"] - peak[0]) <= 5 and abs(first_pick["hi"] - peak[1]) <= 5


def first_pick(out):
    return out["confirm"][0]["setting"] if out["confirm"] else None


def main(argv=None):
    ap = argparse.ArgumentParser()
    ap.add_argument("out")
    ap.add_argument("--skip-9b", action="store_true")
    args = ap.parse_args(argv)
    root = Path(args.out)
    results = {"scripted": {}, "9b": {}}
    studies = {name: make_study(root, name, peak, seed=i + 1) for i, (name, peak) in enumerate(PLANTED.items())}
    null_study = make_study(root, "null", None, seed=99)

    for name, study in studies.items():
        hits = []
        for seed in range(1, 11):
            out = run_study(study, root / "runs" / f"scripted-{name}-{seed}", ScriptedSearch(SPACE, CONSTRAINTS, "higher", seed))
            fp = first_pick(out)
            hits.append(bool(fp and hit(fp, PLANTED[name])))
        results["scripted"][name] = hits
        print("scripted", name, sum(hits), "of 10", flush=True)

    null_flags = []
    for seed in range(1, 21):
        out = run_study(null_study, root / "runs" / f"null-{seed}", ScriptedSearch(SPACE, CONSTRAINTS, "higher", seed))
        null_flags.append(any(r["flagged"] for r in out["confirm"]))
    print("null runs with a confirmed pick:", sum(null_flags), "of 20", flush=True)

    if not args.skip_9b:
        from local_sci.llm_lane import NineBExperimenter, ServerLease
        with ServerLease(status=lambda m: print("status:", m, flush=True)) as client:
            for name, study in studies.items():
                hits = []
                for seed in range(1, 11):
                    exp = NineBExperimenter(client, (study / "question.md").read_text(), (study / "measure.md").read_text(),
                                            SPACE, CONSTRAINTS, "higher", seed * 1000)
                    try:
                        out = run_study(study, root / "runs" / f"9b-{name}-{seed}", exp)
                        fp = first_pick(out)
                        hits.append(bool(fp and hit(fp, PLANTED[name])))
                    except Exception as err:  # a failed run counts as a miss and is logged
                        print("9b run failed:", name, seed, err, flush=True)
                        hits.append(False)
                results["9b"][name] = hits
                print("9b", name, sum(hits), "of 10", flush=True)

    lines = ["# local-sci bench", "",
             "A run hits when its first pick is within 5 bp of the planted best on both window edges; 10 runs per study, budget 4 rounds of 5.", "",
             "| Study | Planted best | Scripted hits | 9B hits |", "|---|---|---|---|"]
    for name, peak in PLANTED.items():
        nine = results["9b"].get(name)
        lines.append(f"| {name} | {peak[0]}-{peak[1]} | {sum(results['scripted'][name])} of 10 | "
                     f"{'not run' if nine is None else f'{sum(nine)} of 10'} |")
    lines += ["", f"Null study (no effect anywhere), scripted search: {sum(null_flags)} of 20 runs confirmed a pick "
                  f"({sum(null_flags) / 20:.0%}); the pass bar is 5% or less.", ""]
    (root / "bench.md").write_text("\n".join(lines))
    print((root / "bench.md").read_text())
    return 0


if __name__ == "__main__":
    sys.exit(main())
