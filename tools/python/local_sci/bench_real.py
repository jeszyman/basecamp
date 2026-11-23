"""Real-data bench: does the 9B find a window planted in real cfDNA fragment lengths?

Each study starts from the fragment-length frequencies (bp_50 to bp_450) of the 45
selected samples of the sarcoma cohort (28 sarcomas, 17 healthy). The samples are
reassigned at random to 28 "case" and 17 "reference" samples, so the groups differ
only by chance. In each case sample, a share PLANT_SHARE of the fragments is moved
into the planted window [lo, hi]: the frequencies become (1 - s) * f + s * g, where
g is f restricted to the window and scaled to sum to 1, so the length shape inside
the window is kept. The measure for a window is a case sample's share of fragments
inside it minus the mean share of the reference samples (0 at the full range 50-450).

The known best of a study is the window with the highest median measure over all 28
cases, found by evaluating every window in the space. Where fragments are sparse
(above about 220 bp) neighboring upper edges score almost alike, so the exact
known-best window is not recoverable from half the samples, and runs are scored by
value: a run hits when its first pick's median measure over all 28 cases is at least
that of the window an exhaustive search of the run's search half would pick. Every
run uses the same split of the samples (split seed 1), so that bar is the best any
searcher can reach on the split. The bench also reports each first pick's value as a
share of the known best's. A seventh study plants nothing; the
scripted search runs it 20 times, and a run counts as a false flag when any pick is
confirmed.

    python -m local_sci.bench_real OUT_DIR [--skip-9b]
"""
import argparse
import csv
import random
import statistics
import sys
from pathlib import Path

from local_sci.bench import CONSTRAINTS, SPACE, first_pick
from local_sci.experimenter import ScriptedSearch, run_study
from local_sci.harness import split_samples
from local_sci.space import in_space

SOURCE = Path("/mnt/gcs/lilli/ewing/analysis_bundle/permutation_bootstrap_jeff")
LENGTHS = range(50, 451)
PLANT_SHARE = 0.2
N_CASES = 28
RUNS = 5


def draw_plants(n=6, seed=11):
    """Planted windows (lo, hi) on the 5 bp grid: lo 100-200, hi 30-160 bp above lo and at most 260."""
    rng = random.Random(seed)
    plants = {}
    while len(plants) < n:
        lo = rng.randrange(100, 201, 5)
        hi = rng.randrange(lo + 30, min(lo + 160, 260) + 1, 5)
        plants[f"real-{len(plants) + 1}"] = (lo, hi)
    return plants


PLANTED = draw_plants()


def read_source(source=SOURCE):
    """{sample: [frequency at 50..450 bp]} for the samples marked cancer or healthy."""
    with open(source / "sample_selection_metadata.tsv", newline="") as fh:
        keep = {r["sample"] for r in csv.DictReader(fh, delimiter="\t") if r["selected"] in ("cancer", "healthy")}
    with open(source / "per_bam_fragment_frequencies.tsv", newline="") as fh:
        return {r["sample"]: [float(r[f"bp_{n}"]) for n in LENGTHS] for r in csv.DictReader(fh, delimiter="\t")
                if r["sample"] in keep}


def plant(freqs, window, share):
    """(1 - share) * freqs + share * (freqs inside the window, scaled to sum to 1)."""
    lo, hi = window
    inside = [f if lo <= n <= hi else 0.0 for n, f in zip(LENGTHS, freqs)]
    total = sum(inside)
    return [(1 - share) * f + share * g / total for f, g in zip(freqs, inside)]


def assign(samples, seed):
    """{sample: "case" or "ref"}: N_CASES cases drawn at random, the rest reference."""
    order = sorted(samples)
    random.Random(seed).shuffle(order)
    return {s: ("case" if i < N_CASES else "ref") for i, s in enumerate(order)}


MEASURE = '''"""Real-data bench measure: a case sample's share of fragments in [lo, hi] minus the reference mean."""
import csv
import os
import statistics

CASE_LABEL = "case"
NO_CHANGE_VALUE = 0.0
NO_CHANGE = {"name": "50-450", "lo": 50, "hi": 450}
SPACE = {"lo": (50, 300, 5), "hi": (70, 450, 5)}
CONSTRAINTS = ["lo < hi"]
BETTER = "higher"


def load(data_dir):
    with open(os.path.join(data_dir, "freqs.tsv"), newline="") as fh:
        rows = list(csv.DictReader(fh, delimiter="\\t"))
    cum = {}
    for r in rows:
        running, out = 0.0, [0.0]
        for n in range(50, 451):
            running += float(r[f"bp_{n}"])
            out.append(running)
        cum[r["sample"]] = out
    return cum, {r["sample"]: r["label"] for r in rows}


def share(cum, sample, lo, hi):
    c = cum[sample.split("#")[0]]
    return c[hi - 49] - c[lo - 50]


def measure(data, labels, setting):
    lo, hi = setting["lo"], setting["hi"]
    if (lo, hi) == (50, 450):
        return {k: 0.0 for k, label in labels.items() if label == CASE_LABEL}
    ref = statistics.mean(share(data, k, lo, hi) for k, label in labels.items() if label != CASE_LABEL)
    return {k: share(data, k, lo, hi) - ref for k, label in labels.items() if label == CASE_LABEL}
'''

QUESTION = ("Which fragment-length window [lo, hi] bp best separates the case samples from the reference samples "
            "by the share of each sample's cfDNA fragments whose length falls inside the window?\n")
MEASURE_MD = """## Quantity

For one case sample and one window [lo, hi] bp: the sample's share of fragments with length from lo to hi bp, minus the mean of the same share over the reference samples. Higher is better.

## How it is computed

From each sample's fragment-length frequencies at 50 to 450 bp, which sum to 1.

## No-change value

0, at the full range 50-450, where every sample's share is 1.
"""


def make_study(root, name, window, source_freqs, seed):
    study = root / "studies" / name
    data = study / "data"
    data.mkdir(parents=True, exist_ok=True)
    labels = assign(source_freqs, seed)
    with open(data / "freqs.tsv", "w", newline="") as fh:
        w = csv.writer(fh, delimiter="\t", lineterminator="\n")
        w.writerow(["sample", "label"] + [f"bp_{n}" for n in LENGTHS])
        for s in sorted(source_freqs):
            f = source_freqs[s]
            if window is not None and labels[s] == "case":
                f = plant(f, window, PLANT_SHARE)
            w.writerow([s, labels[s]] + [f"{v:.8g}" for v in f])
    (study / "measure.py").write_text(MEASURE)
    (study / "question.md").write_text(QUESTION)
    (study / "measure.md").write_text(MEASURE_MD)
    (study / "data.txt").write_text(str(data) + "\n")
    return study


def value_at(study, setting, labels=None):
    """Median measure over the case samples (all of them unless labels are given) at one window."""
    from local_sci.harness import load_measure
    mod = load_measure(study / "measure.py")
    data, all_labels = mod.load(str(study / "data"))
    return statistics.median(mod.measure(data, labels or all_labels, setting).values())


def grid_best(study, labels=None):
    """The window with the highest median measure over the case samples, searching every window in SPACE."""
    from local_sci.harness import load_measure
    mod = load_measure(study / "measure.py")
    data, all_labels = mod.load(str(study / "data"))
    labels = labels or all_labels
    best, best_value = None, None
    for lo in range(SPACE["lo"][0], SPACE["lo"][1] + 1, 5):
        for hi in range(SPACE["hi"][0], SPACE["hi"][1] + 1, 5):
            setting = {"lo": lo, "hi": hi}
            if not in_space(setting, SPACE, CONSTRAINTS) or (lo, hi) == (50, 450):
                continue
            value = statistics.median(mod.measure(data, labels, setting).values())
            if best_value is None or value > best_value:
                best, best_value = setting, value
    return best, best_value


def score(study, pick, bar, best_value):
    """(hit, share of the known best's value) for a run's first pick; a run with no pick scores (False, 0)."""
    if pick is None:
        return False, 0.0
    v = value_at(study, {"lo": pick["lo"], "hi": pick["hi"]})
    return v >= bar, v / best_value


def main(argv=None):
    ap = argparse.ArgumentParser()
    ap.add_argument("out")
    ap.add_argument("--skip-9b", action="store_true")
    ap.add_argument("--model", default="qwen3.5-9b")
    args = ap.parse_args(argv)
    root = Path(args.out)
    source = read_source()
    studies = {name: make_study(root, name, w, source, seed=i + 1) for i, (name, w) in enumerate(PLANTED.items())}
    null_study = make_study(root, "null", None, source, seed=99)

    from local_sci.harness import load_measure
    known, known_value, bar, disc_pick = {}, {}, {}, {}
    for name, study in studies.items():
        known[name], known_value[name] = grid_best(study)
        _, labels = load_measure(study / "measure.py").load(str(study / "data"))
        discovery, _ = split_samples(labels, 1)
        disc_pick[name], _ = grid_best(study, discovery)
        bar[name] = value_at(study, disc_pick[name])
        print(name, "planted", PLANTED[name], "known best", known[name], f"median {known_value[name]:.4f};",
              "search-half best", disc_pick[name], f"median on all cases {bar[name]:.4f}", flush=True)

    results = {"scripted": {}, "9b": {}}
    shares = {"scripted": [], "9b": []}
    for name, study in studies.items():
        hits = []
        for seed in range(1, RUNS + 1):
            out = run_study(study, root / "runs" / f"scripted-{name}-{seed}", ScriptedSearch(SPACE, CONSTRAINTS, "higher", seed))
            h, s = score(study, first_pick(out), bar[name], known_value[name])
            hits.append(h)
            shares["scripted"].append(s)
        results["scripted"][name] = hits
        print("scripted", name, sum(hits), f"of {RUNS}", flush=True)

    null_flags = []
    for seed in range(1, 21):
        out = run_study(null_study, root / "runs" / f"null-{seed}", ScriptedSearch(SPACE, CONSTRAINTS, "higher", seed))
        null_flags.append(any(r["flagged"] for r in out["confirm"]))
    print("null runs with a confirmed pick:", sum(null_flags), "of 20", flush=True)

    if not args.skip_9b:
        from local_sci.llm_lane import NineBExperimenter, ServerLease
        with ServerLease(model=args.model, status=lambda m: print("status:", m, flush=True)) as client:
            for name, study in studies.items():
                hits = []
                for seed in range(1, RUNS + 1):
                    exp = NineBExperimenter(client, QUESTION, MEASURE_MD, SPACE, CONSTRAINTS, "higher", seed * 1000,
                                            baseline={"name": "50-450", "value": 0.0})
                    try:
                        out = run_study(study, root / "runs" / f"{args.model}-{name}-{seed}", exp)
                        h, s = score(study, first_pick(out), bar[name], known_value[name])
                    except Exception as err:  # a failed run counts as a miss and is logged
                        print(args.model, "run failed:", name, seed, err, flush=True)
                        h, s = False, 0.0
                    hits.append(h)
                    shares["9b"].append(s)
                results["9b"][name] = hits
                print(args.model, name, sum(hits), f"of {RUNS}", flush=True)

    lines = ["# local-sci real-data bench", "",
             f"Sarcoma-cohort fragment lengths, samples reassigned at random to {N_CASES} case and "
             f"{len(source) - N_CASES} reference; {PLANT_SHARE:.0%} of each case sample's fragments moved into the "
             f"planted window. A run hits when its first pick's median over all case samples is at least that of the "
             f"window an exhaustive search of the search half picks (the bar). {RUNS} runs per study, budget 4 rounds of 5.", "",
             f"| Study | Planted | Known best (median) | Search-half best (median on all cases) | Scripted hits | {args.model} hits |",
             "|---|---|---|---|---|---|"]
    for name, w in PLANTED.items():
        k, d = known[name], disc_pick[name]
        cells = [f"{sum(results[x][name])} of {RUNS}" if name in results[x] else "not run" for x in ("scripted", "9b")]
        lines.append(f"| {name} | {w[0]}-{w[1]} | {k['lo']}-{k['hi']} ({known_value[name]:.4f}) | "
                     f"{d['lo']}-{d['hi']} ({bar[name]:.4f}) | {cells[0]} | {cells[1]} |")
    totals = [f"{sum(sum(v) for v in results[x].values())} of {RUNS * len(PLANTED)}" if results[x] else "not run"
              for x in ("scripted", "9b")]
    lines.append(f"| all | | | | {totals[0]} | {totals[1]} |")
    for x, label in (("scripted", "Scripted"), ("9b", args.model)):
        if shares[x]:
            lines.append(f"\n{label}: first pick's median as a share of the known best's, median over runs "
                         f"{statistics.median(shares[x]):.3f} (range {min(shares[x]):.3f} to {max(shares[x]):.3f}).")
    lines += ["", f"Null study (nothing planted), scripted search: {sum(null_flags)} of 20 runs confirmed a pick.", ""]
    (root / "bench.md").write_text("\n".join(lines))
    print((root / "bench.md").read_text())
    return 0


if __name__ == "__main__":
    sys.exit(main())
