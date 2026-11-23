"""Net signal-to-noise gain of a cfDNA fragment-length window for the sarcoma cohort.

The measure of ~/OpenResearch/fragwindow-breakeven/breakeven.py written to the
local-sci interface: for each cancer sample, e(w) * sqrt(r(w)), where r(w) is the
sample's fraction of fragments inside the window and e(w) the tumor-fraction gain
inside the window, treating the sample as its ichorCNA tumor fraction of tumor
fragments plus the mean healthy length distribution.
"""
import csv
import math
import os
import statistics

CASE_LABEL = "cancer"
NO_CHANGE_VALUE = 1.0
NO_CHANGE = {"name": "50-450", "lo": 50, "hi": 450}
SPACE = {"lo": (50, 300, 5), "hi": (70, 450, 5)}
CONSTRAINTS = ["lo < hi"]
BETTER = "higher"
SETTINGS = [
    {"name": "50-450", "lo": 50, "hi": 450},
    {"name": "55-130", "lo": 55, "hi": 130},
    {"name": "55-150", "lo": 55, "hi": 150},
    {"name": "55-170", "lo": 55, "hi": 170},
    {"name": "55-200", "lo": 55, "hi": 200},
]


def _read(path):
    with open(path, newline="") as fh:
        return list(csv.DictReader(fh, delimiter="\t"))


def load(data_dir):
    meta = {r["sample"]: r for r in _read(os.path.join(data_dir, "sample_selection_metadata.tsv"))}
    freqs = {r["sample"]: r for r in _read(os.path.join(data_dir, "per_bam_fragment_frequencies.tsv"))}
    labels = {s: meta[s]["selected"] for s in freqs if s in meta}
    data = {
        "freqs": {s: freqs[s] for s in labels},
        "tf": {s: float(meta[s]["tf"]) for s in labels},
    }
    return data, labels


def measure(data, labels, setting):
    lo, hi = setting["lo"], setting["hi"]

    def inside(sample):
        row = data["freqs"][sample.split("#")[0]]
        return sum(float(row[f"bp_{length}"]) for length in range(lo, hi + 1))

    healthy = [s for s, label in labels.items() if label == "healthy"]
    healthy_inside = statistics.mean(inside(s) for s in healthy)
    out = {}
    for sample, label in labels.items():
        if label != CASE_LABEL:
            continue
        tf = data["tf"][sample.split("#")[0]]
        kept = inside(sample)
        if kept <= 0 or tf <= 0:
            out[sample] = float("nan")
            continue
        gain = (1 - (1 - tf) * healthy_inside / kept) / tf
        out[sample] = gain * math.sqrt(kept)
    return out
