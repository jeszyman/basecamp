"""Short-fragment window against the DELFI short window (100-150 bp), sarcoma cohort.

For each cancer sample: z(w) - z(100-150), where z(w) is the sample's share of
fragments with length in [lo, hi] bp as a z-score against the healthy samples in
labels (their mean and sample standard deviation).
"""
import csv
import os
import statistics

CASE_LABEL = "cancer"
NO_CHANGE_VALUE = 0.0
NO_CHANGE = {"name": "100-150", "lo": 100, "hi": 150}
SPACE = {"lo": (50, 200, 5), "hi": (70, 300, 5)}
CONSTRAINTS = ["lo < hi"]
BETTER = "higher"
REFERENCE = (100, 150)


def _read(path):
    with open(path, newline="") as fh:
        return list(csv.DictReader(fh, delimiter="\t"))


def load(data_dir):
    meta = {r["sample"]: r for r in _read(os.path.join(data_dir, "sample_selection_metadata.tsv"))}
    freqs = {r["sample"]: r for r in _read(os.path.join(data_dir, "per_bam_fragment_frequencies.tsv"))}
    labels = {s: meta[s]["selected"] for s in freqs if s in meta and meta[s]["selected"] in ("cancer", "healthy")}
    return {"freqs": {s: freqs[s] for s in labels}}, labels


def _z_scores(data, labels, lo, hi):
    def share(key):
        row = data["freqs"][key.split("#")[0]]
        return sum(float(row[f"bp_{length}"]) for length in range(lo, hi + 1))

    healthy = [share(k) for k, label in labels.items() if label == "healthy"]
    mean, sd = statistics.mean(healthy), statistics.stdev(healthy)
    if sd <= 0:
        return {k: float("nan") for k, label in labels.items() if label == CASE_LABEL}
    return {k: (share(k) - mean) / sd for k, label in labels.items() if label == CASE_LABEL}


def measure(data, labels, setting):
    window = _z_scores(data, labels, setting["lo"], setting["hi"])
    reference = _z_scores(data, labels, *REFERENCE)
    return {k: window[k] - reference[k] for k in window}
