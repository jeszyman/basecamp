"""UXM deconvolution settings against the published UXM default, second-line breast cohort.

For each patient: z(setting) - z(default), where z is the patient's summed
epithelial fraction from UXM deconvolution as a z-score against the healthy
donors in labels (their mean and sample standard deviation). The deconvolution
follows uxm deconv (UXM_deconv/src/deconv.py): per marker, the share of reads
with at least rlen CpGs in the marker whose methylated fraction is at most 0.25,
then non-negative least squares against the atlas with each marker weighted by
its read count, and the mixture scaled to sum to 1.
"""
import csv
import gzip
import os
import statistics

import numpy as np
from scipy import optimize

CASE_LABEL = "cancer"
NO_CHANGE_VALUE = 0.0
NO_CHANGE = {"name": "25-4-1", "markers": 25, "rlen": 4, "breast": 1}
SPACE = {"markers": (10, 250, 5), "rlen": (3, 8, 1), "breast": (0, 1, 1)}
CONSTRAINTS = []
BETTER = "higher"

# The 36 cell types of the published U25 atlas (Atlas.U25.l4.hg38.tsv); the U250
# atlas adds Bone-Osteob, Colon-Fibro, Dermal-Fibro and Gallbladder, left out here.
CELL_TYPES = [
    "Adipocytes", "Bladder-Ep", "Blood-B", "Blood-Granul", "Blood-Mono+Macro", "Blood-NK", "Blood-T",
    "Breast-Basal-Ep", "Breast-Luminal-Ep", "Colon-Ep", "Endothel", "Epid-Kerat", "Eryth-prog",
    "Fallopian-Ep", "Gastric-Ep", "Head-Neck-Ep", "Heart-Cardio", "Heart-Fibro", "Kidney-Ep", "Liver-Hep",
    "Lung-Ep-Alveo", "Lung-Ep-Bron", "Neuron", "Oligodend", "Ovary-Ep", "Pancreas-Acinar",
    "Pancreas-Alpha", "Pancreas-Beta", "Pancreas-Delta", "Pancreas-Duct", "Megakaryocytes",
    "Prostate-Ep", "Skeletal-Musc", "Small-Int-Ep", "Smooth-Musc", "Thyroid-Ep",
]
BREAST = {"Breast-Basal-Ep", "Breast-Luminal-Ep"}
EPITHELIAL = {t for t in CELL_TYPES if "-Ep" in t}
RLENS = range(3, 9)

_FRACTION = {}


def load(data_dir):
    with open(os.path.join(data_dir, "atlas_ranked.tsv"), newline="") as fh:
        atlas = [r for r in csv.DictReader(fh, delimiter="\t") if r["target"] in CELL_TYPES]
    names = [r["name"] for r in atlas]
    rows_of = {}
    for i, n in enumerate(names):
        rows_of.setdefault(n, []).append(i)
    with open(os.path.join(data_dir, "labels.tsv"), newline="") as fh:
        labels = {r["sample"]: r["label"] for r in csv.DictReader(fh, delimiter="\t")}
    counts = {}
    with gzip.open(os.path.join(data_dir, "homog.tsv.gz"), "rt", newline="") as fh:
        for r in csv.DictReader(fh, delimiter="\t"):
            if r["name"] not in rows_of:
                continue
            key = (r["sample"], int(r["rlen"]))
            if key not in counts:
                counts[key] = np.zeros((len(names), 3))
            for i in rows_of[r["name"]]:
                counts[key][i] = (int(r["U"]), int(r["X"]), int(r["M"]))
    data = {
        "name": np.array(names),
        "target": np.array([r["target"] for r in atlas]),
        "rank": np.array([int(r["rank"]) for r in atlas]),
        "ref": np.array([[float("nan") if r[t] == "NA" else float(r[t]) for t in CELL_TYPES] for r in atlas]),
        "counts": counts,
    }
    return data, {s: lab for s, lab in labels.items() if all((s, l) in counts for l in RLENS)}


def mixture(data, sample, markers, rlen, breast):
    """Cell-type fractions {cell type: fraction} of one sample under one setting."""
    types = [t for t in CELL_TYPES if breast or t not in BREAST]
    cols = [CELL_TYPES.index(t) for t in types]
    rows = np.isin(data["target"], types) & (data["rank"] < markers)
    if "name" in data:
        # uxm deconv keeps the first atlas row of a region listed for two cell types
        _, first = np.unique(data["name"][rows], return_index=True)
        keep_first = np.zeros(rows.sum(), dtype=bool)
        keep_first[first] = True
        rows[np.flatnonzero(rows)[~keep_first]] = False
    uxm = data["counts"][(sample, rlen)][rows]
    ref = data["ref"][np.ix_(rows, cols)]
    total = uxm.sum(axis=1)
    # uxm deconv drops markers with no reads and markers with a missing atlas value
    keep = (total > 0) & ~np.isnan(ref).any(axis=1)
    weight = total[keep]
    u_share = uxm[keep, 0] / weight
    ref = ref[keep]
    fit, _ = optimize.nnls(ref * weight[:, None], u_share * weight)
    fit = fit / fit.sum() if fit.sum() > 0 else fit * np.nan
    return dict(zip(types, (float(f) for f in fit)))


def epithelial_fraction(data, sample, markers, rlen, breast):
    """Summed epithelial fraction of one sample under one setting (cached)."""
    key = (sample, markers, rlen, breast)
    if key not in _FRACTION:
        fit = mixture(data, sample, markers, rlen, breast)
        _FRACTION[key] = sum(f for t, f in fit.items() if t in EPITHELIAL)
    return _FRACTION[key]


def _z_scores(data, labels, markers, rlen, breast):
    def frac(key):
        return epithelial_fraction(data, key.split("#")[0], markers, rlen, breast)

    healthy = [frac(k) for k, label in labels.items() if label == "healthy"]
    mean, sd = statistics.mean(healthy), statistics.stdev(healthy)
    if not sd > 0:
        return {k: float("nan") for k, label in labels.items() if label == CASE_LABEL}
    return {k: (frac(k) - mean) / sd for k, label in labels.items() if label == CASE_LABEL}


def measure(data, labels, setting):
    tried = _z_scores(data, labels, setting["markers"], setting["rlen"], setting["breast"])
    default = _z_scores(data, labels, NO_CHANGE["markers"], NO_CHANGE["rlen"], NO_CHANGE["breast"])
    return {k: tried[k] - default[k] for k in tried}
