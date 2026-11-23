"""Planted-tissue bench: which tissue sheds extra DNA into the plasma of the case group?

Design: ~/repos/basecamp/docs/specs/2026-11-23-planted-tissue-study.md.

The real UXM marker read counts of the 55 breast-cohort EM-seq libraries are
reassigned at random to 28 case and 27 reference libraries. In each case library,
DNA of one planted cell type is added in silico at share f: at each marker the share
of fully unmethylated reads becomes (1 - f) times the library's share plus f times the
atlas value of the planted cell type, with the read count unchanged. One study per
non-blood cell type (29), plus one with nothing planted. f is the smallest multiple of
0.005 at which the planted type's mean excess in cases reaches 2 reference standard
deviations. Libraries are deconvolved with the uxm-matched fit of the uxm-settings
study (25 markers per cell type, 4 CpGs per read).

The setting is target, 0 to 45: 0 is all cell types (no change), 1 to 45 are the 36
cell types and 9 families in a seeded shuffled order. The measure for a case library
is the mean over the target's cell types of its fraction minus the reference mean, in
percentage points. Searchers: the 9B with names, the 9B with integers only, the
compass search, and a family-first script; 2 runs per study each. A run hits when its
first pick is the planted cell type; studies where an exhaustive search of the search
half misses it are reported and left out of the counts.

    python -m local_sci.bench_tissue OUT_DIR [--skip-9b]
"""
import argparse
import csv
import importlib.util
import math
import random
import statistics
import sys
from pathlib import Path

import numpy as np

from local_sci.experimenter import ScriptedSearch, run_study
from local_sci.harness import load_measure, split_samples

HERE = Path(__file__).parent
TABLES = Path("/mnt/data/projects/uxm-study/tables")
N_CASES, RUNS, F_STEP, F_MAX = 28, 2, 0.005, 0.3

_spec = importlib.util.spec_from_file_location("uxm_measure", HERE / "studies" / "uxm-settings" / "measure.py")
UXM = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(UXM)
CELL_TYPES = UXM.CELL_TYPES

FAMILIES = {
    "Blood family": ["Blood-B", "Blood-Granul", "Blood-Mono+Macro", "Blood-NK", "Blood-T", "Eryth-prog", "Megakaryocytes"],
    "Pancreas family": ["Pancreas-Acinar", "Pancreas-Alpha", "Pancreas-Beta", "Pancreas-Delta", "Pancreas-Duct"],
    "Lung family": ["Lung-Ep-Alveo", "Lung-Ep-Bron"],
    "Heart family": ["Heart-Cardio", "Heart-Fibro"],
    "Breast family": ["Breast-Basal-Ep", "Breast-Luminal-Ep"],
    "Brain family": ["Neuron", "Oligodend"],
    "Gut epithelium family": ["Colon-Ep", "Gastric-Ep", "Small-Int-Ep"],
    "Other epithelium family": ["Bladder-Ep", "Epid-Kerat", "Fallopian-Ep", "Head-Neck-Ep", "Kidney-Ep", "Liver-Hep",
                                "Ovary-Ep", "Prostate-Ep", "Thyroid-Ep"],
    "Connective tissue and muscle family": ["Adipocytes", "Endothel", "Skeletal-Musc", "Smooth-Musc"],
}
PLANTABLE = [t for t in CELL_TYPES if t not in FAMILIES["Blood family"]]


def targets(seed=23):
    """{integer: (name, member cell types)}: 0 is all cell types, 1-45 the cell types and families shuffled."""
    items = [(t, [t]) for t in CELL_TYPES] + list(FAMILIES.items())
    random.Random(seed).shuffle(items)
    out = {0: ("All cell types", list(CELL_TYPES))}
    out.update({i + 1: item for i, item in enumerate(items)})
    return out


TARGETS = targets()
INDEX = {name: i for i, (name, _) in TARGETS.items()}


def plant_counts(counts, ref_column, f):
    """Marker counts [U, X, M] after adding share f of a cell type whose atlas U shares are ref_column."""
    out = counts.copy()
    total = counts.sum(axis=1)
    ok = (total > 0) & ~np.isnan(ref_column)
    u = np.where(ok, counts[:, 0] / np.where(total > 0, total, 1), 0.0)
    new_u = (1 - f) * u + f * np.nan_to_num(ref_column)
    rest = counts[:, 1] + counts[:, 2]
    new_rest = total * (1 - new_u)
    x_share = np.where(rest > 0, counts[:, 1] / np.where(rest > 0, rest, 1), 0.0)
    out[ok, 0] = (total * new_u)[ok]
    out[ok, 1] = (new_rest * x_share)[ok]
    out[ok, 2] = (new_rest * (1 - x_share))[ok]
    return out


def fractions(data, sample, counts):
    """Deconvolved cell-type fractions of one library from its rlen-4 marker counts."""
    d = dict(data)
    d["counts"] = {(sample, 4): counts}
    return UXM.mixture(d, sample, 25, 4, 1)


def assign(samples, seed):
    order = sorted(samples)
    random.Random(seed).shuffle(order)
    return {s: ("case" if i < N_CASES else "ref") for i, s in enumerate(order)}


def calibrate(data, base, labels, planted):
    """(f, signal in reference SDs, case fractions) at the smallest f reaching 2 reference SDs."""
    col = data["ref"][:, CELL_TYPES.index(planted)]
    refs = [base[s][planted] for s, l in labels.items() if l == "ref"]
    mean_ref, sd_ref = statistics.mean(refs), statistics.stdev(refs)
    cases = [s for s, l in labels.items() if l == "case"]
    f, signal, planted_fr = 0.0, 0.0, {}
    while f < F_MAX:
        f = round(f + F_STEP, 4)
        planted_fr = {s: fractions(data, s, plant_counts(data["counts"][(s, 4)], col, f)) for s in cases}
        signal = (statistics.mean(planted_fr[s][planted] for s in cases) - mean_ref) / sd_ref
        if signal >= 2:
            return f, signal, planted_fr
    return f, signal, planted_fr


MEASURE = '''"""Planted-tissue bench measure: mean excess, over the target's cell types, of a case library's fraction."""
import csv
import os
import statistics

CASE_LABEL = "case"
NO_CHANGE_VALUE = 0.0
NO_CHANGE = {{"name": "0", "target": 0}}
SPACE = {{"target": (0, 45, 1)}}
CONSTRAINTS = []
BETTER = "higher"
MEMBERS = {members!r}


def load(data_dir):
    with open(os.path.join(data_dir, "fractions.tsv"), newline="") as fh:
        rows = list(csv.DictReader(fh, delimiter="\\t"))
    return {{r["sample"]: {{k: float(v) for k, v in r.items() if k not in ("sample", "label")}} for r in rows}}, \\
        {{r["sample"]: r["label"] for r in rows}}


def measure(data, labels, setting):
    t = setting["target"]
    cases = [k for k, label in labels.items() if label == CASE_LABEL]
    if t == 0:
        return {{k: 0.0 for k in cases}}
    refs = [k for k, label in labels.items() if label != CASE_LABEL]
    members = MEMBERS[t]
    ref_mean = {{c: statistics.mean(data[k.split("#")[0]][c] for k in refs) for c in members}}
    return {{k: 100 * statistics.mean(data[k.split("#")[0]][c] - ref_mean[c] for c in members) for k in cases}}
'''

QUESTION = "Which tissue's DNA is raised in the plasma of the case group, compared with the reference group?\n"


def measure_md(named):
    lines = ["## Quantity", "",
             "For one case sample and one target: the sample's deconvolved fraction of plasma DNA from the target's cell "
             "type(s) minus the mean fraction among the reference samples, in percentage points. For a target that is a "
             "family of cell types, the value is the mean of that difference over the family's members. Higher is better.",
             "", "## Targets", ""]
    if named:
        lines.append("Target 0 is all cell types together (the no-change setting). Targets 1 to 45:")
        for i in range(1, 46):
            name, members = TARGETS[i]
            lines.append(f"- {i}: {name}" + (f" ({', '.join(members)})" if len(members) > 1 else ""))
    else:
        lines.append("Target 0 is all cell types together (the no-change setting). Targets 1 to 45 are 36 single cell "
                     "types and 9 families of related cell types, identified by number only.")
    lines += ["", "## No-change value", "", "0, at target 0."]
    return "\n".join(lines) + "\n"


def make_study(root, name, data, base, samples, planted, seed):
    study = root / "studies" / name
    (study / "data").mkdir(parents=True, exist_ok=True)
    labels = assign(samples, seed)
    f, signal, planted_fr = (0.0, 0.0, {}) if planted is None else calibrate(data, base, labels, planted)
    with open(study / "data" / "fractions.tsv", "w", newline="") as fh:
        w = csv.writer(fh, delimiter="\t", lineterminator="\n")
        w.writerow(["sample", "label"] + CELL_TYPES)
        for s in sorted(samples):
            fr = planted_fr.get(s, base[s])
            w.writerow([s, labels[s]] + [f"{fr[c]:.8g}" for c in CELL_TYPES])
    (study / "measure.py").write_text(MEASURE.format(members={i: m for i, (_, m) in TARGETS.items()}))
    (study / "question.md").write_text(QUESTION)
    (study / "measure.md").write_text(measure_md(True))
    (study / "data.txt").write_text(str(study / "data") + "\n")
    return study, f, signal


def exhaustive_pick(study):
    """The target with the highest median over the search half's case samples (split seed 1)."""
    mod = load_measure(str(study / "measure.py"))
    data, labels = mod.load(str(study / "data"))
    discovery, _ = split_samples(labels, 1)
    return max(range(1, 46), key=lambda t: statistics.median(mod.measure(data, discovery, {"target": t}).values()))


class FamilyFirstSearch:
    """Tries all 9 families, then the members of the families in order of their result, best first."""

    def __init__(self):
        self.families = [INDEX[f] for f in FAMILIES]

    def propose(self, history, k, budget_left):
        tried = {h["setting"]["target"] for h in history}
        order = [t for t in self.families if t not in tried]
        if not order:
            ranked = sorted((h for h in history if h["setting"]["target"] in self.families),
                            key=lambda h: -(h["row"]["median"] if h["row"]["median"] is not None else -math.inf))
            order = [INDEX[c] for h in ranked for c in TARGETS[h["setting"]["target"]][1] if INDEX[c] not in tried]
        out = []
        for t in order[:min(k, budget_left)]:
            out.append({"target": t, "name": str(t), "reason": "family first, then the members of the best family"})
        return out, {"scripted": True}

    def pick(self, history):
        ranked = sorted((h for h in history if h["row"]["median"] is not None), key=lambda h: -h["row"]["median"])
        return [{"name": h["setting"]["name"], "reason": "top discovery median"} for h in ranked[:3]], {"scripted": True}

    def narrate(self, history):
        return "A family-first script: all families, then the members of the best families in order.", {}


def sign_test(a, b):
    """Two-sided sign test over studies on per-study hit counts: (studies a better, b better, p)."""
    wins = sum(x > y for x, y in zip(a, b))
    losses = sum(y > x for x, y in zip(a, b))
    n = wins + losses
    if n == 0:
        return wins, losses, 1.0
    tail = sum(math.comb(n, i) for i in range(0, min(wins, losses) + 1)) / 2 ** n
    return wins, losses, min(1.0, 2 * tail)


def main(argv=None):
    ap = argparse.ArgumentParser()
    ap.add_argument("out")
    ap.add_argument("--skip-9b", action="store_true")
    ap.add_argument("--model", default="qwen3.5-9b")
    args = ap.parse_args(argv)
    root = Path(args.out)
    data, real = UXM.load(str(TABLES))
    samples = sorted(real)
    base = {s: fractions(data, s, data["counts"][(s, 4)]) for s in samples}

    studies, info = {}, {}
    for i, planted in enumerate(PLANTABLE):
        name = f"tissue-{i + 1:02d}"
        study, f, signal = make_study(root, name, data, base, samples, planted, seed=i + 1)
        pick = exhaustive_pick(study)
        studies[name] = study
        info[name] = {"planted": planted, "target": INDEX[planted], "f": f, "signal": signal,
                      "exhaustive": TARGETS[pick][0], "scored": pick == INDEX[planted]}
        print(name, info[name], flush=True)
    null_study, _, _ = make_study(root, "null", data, base, samples, None, seed=99)

    labels_named = {str(i): TARGETS[i][0] for i in TARGETS}
    hits = {k: {} for k in ("9b", "9b-blind", "compass", "family-first")}

    def first_hit(out, name):
        return bool(out["confirm"]) and out["confirm"][0]["setting"]["target"] == info[name]["target"]

    for name, study in studies.items():
        hits["compass"][name] = sum(first_hit(run_study(study, root / "runs" / f"compass-{name}-{s}",
                                                        ScriptedSearch(UXM_SPACE, [], "higher", s)), name)
                                    for s in range(1, RUNS + 1))
        hits["family-first"][name] = sum(first_hit(run_study(study, root / "runs" / f"family-{name}-{s}",
                                                             FamilyFirstSearch()), name) for s in range(1, RUNS + 1))
        print(name, "compass", hits["compass"][name], "family-first", hits["family-first"][name], flush=True)

    null_flags = sum(any(r["flagged"] for r in run_study(null_study, root / "runs" / f"null-{s}",
                                                         ScriptedSearch(UXM_SPACE, [], "higher", s))["confirm"])
                     for s in range(1, 21))
    print("null runs with a confirmed pick:", null_flags, "of 20", flush=True)

    if not args.skip_9b:
        from local_sci.llm_lane import NineBExperimenter, ServerLease
        with ServerLease(model=args.model, status=lambda m: print("status:", m, flush=True)) as client:
            for arm, named in (("9b", True), ("9b-blind", False)):
                for name, study in studies.items():
                    n = 0
                    for s in range(1, RUNS + 1):
                        exp = NineBExperimenter(client, QUESTION, measure_md(named), UXM_SPACE, [], "higher", s * 1000,
                                                baseline={"name": "0", "value": 0.0},
                                                labels=labels_named if named else None)
                        try:
                            n += first_hit(run_study(study, root / "runs" / f"{arm}-{name}-{s}", exp), name)
                        except Exception as err:  # a failed run counts as a miss and is logged
                            print(arm, "run failed:", name, s, err, flush=True)
                    hits[arm][name] = n
                    print(arm, name, n, flush=True)

    scored = [n for n in studies if info[n]["scored"]]
    lines = ["# Planted-tissue bench", "",
             f"{len(studies)} studies, one per non-blood cell type; {RUNS} runs per searcher per study; a run hits when its "
             f"first pick is the planted cell type. Exhaustive search of the search half picked the planted type in "
             f"{len(scored)} of {len(studies)} studies; only those are counted.", "",
             "| Study | Planted | f | Signal (reference SD) | Exhaustive pick | 9B | 9B, numbers only | Compass | Family first |",
             "|---|---|---|---|---|---|---|---|---|"]
    for n in studies:
        i = info[n]
        cells = [str(hits[a].get(n, "not run")) for a in ("9b", "9b-blind", "compass", "family-first")]
        lines.append(f"| {n} | {i['planted']} | {i['f']:.3f} | {i['signal']:.2f} | {i['exhaustive']} | " + " | ".join(cells) + " |")
    total = {a: sum(hits[a][n] for n in scored if n in hits[a]) for a in hits}
    lines += ["", "Hits over counted studies (out of " + str(RUNS * len(scored)) + "): " +
              ", ".join(f"{a} {total[a]}" for a in hits if hits[a]), ""]
    for a, b in (("9b", "compass"), ("9b", "family-first"), ("9b", "9b-blind"), ("family-first", "compass")):
        if hits[a] and hits[b]:
            w, l, p = sign_test([hits[a][n] for n in scored], [hits[b][n] for n in scored])
            lines.append(f"- {a} against {b}: {a} better in {w} studies, {b} better in {l}, sign test p = {p:.3f}")
    lines += ["", f"Null study (nothing planted), compass search: {null_flags} of 20 runs confirmed a pick.", ""]
    (root / "bench.md").write_text("\n".join(lines))
    print((root / "bench.md").read_text())
    return 0


UXM_SPACE = {"target": (0, 45, 1)}

if __name__ == "__main__":
    sys.exit(main())
