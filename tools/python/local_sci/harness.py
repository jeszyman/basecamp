"""Statistics harness for local-sci.

Takes a measure module (CASE_LABEL, NO_CHANGE_VALUE, SETTINGS, load, measure),
runs the measure at each setting, and for each setting reports the median over
case samples, a bootstrap 95% interval of that median, a two-sided bootstrap p
against the no-change value, and a Holm-adjusted p across the settings tested.
The bootstrap follows ~/OpenResearch/fragwindow-breakeven/breakeven.py exactly
(random.Random(1) per setting, 2,000 resamples, sorted-median index interval),
so its numbers reproduce.
"""
import importlib.util
import math
import random
import statistics

N_BOOT = 2000
MIN_SAMPLES = 5
NULL_TOL = 1e-9
ALPHA = 0.05


class NoChangeError(Exception):
    """The no-change setting did not return the no-change value."""


class DuplicateSettingError(Exception):
    """Two settings share a name."""


def load_measure(path):
    spec = importlib.util.spec_from_file_location("local_sci_measure", path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def holm(p):
    order = sorted(range(len(p)), key=lambda i: p[i])
    m = len(p)
    adjusted = [0.0] * m
    running = 0.0
    for rank, i in enumerate(order):
        running = max(running, min(1.0, (m - rank) * p[i]))
        adjusted[i] = running
    return adjusted


def _bootstrap(values, null):
    rng = random.Random(1)
    n = len(values)
    medians = sorted(statistics.median(rng.choices(values, k=n)) for _ in range(N_BOOT))
    low = medians[int(0.025 * N_BOOT)]
    high = medians[int(0.975 * N_BOOT) - 1]
    below = sum(m <= null for m in medians) / N_BOOT
    above = sum(m >= null for m in medians) / N_BOOT
    p = min(1.0, max(1.0 / N_BOOT, 2 * min(below, above)))
    return low, high, p


def run_sweep(mod, data, labels, check_null=True):
    names = [s["name"] for s in mod.SETTINGS]
    if len(set(names)) != len(names):
        raise DuplicateSettingError(f"settings repeat a name: {names}")
    null = float(mod.NO_CHANGE_VALUE)
    rows = []
    for index, setting in enumerate(mod.SETTINGS):
        values = mod.measure(data, labels, setting)
        ordered = [values[s] for s in sorted(values)]
        if index == 0 and check_null:
            off = [v for v in ordered if not (math.isfinite(v) and abs(v - null) <= NULL_TOL)]
            if off:
                raise NoChangeError(
                    f"setting {setting['name']!r} gave {len(off)} of {len(ordered)} values away from {null}"
                )
        finite = [v for v in ordered if math.isfinite(v)]
        row = {
            "setting": setting,
            "values": finite,
            "n": len(finite),
            "n_dropped": len(ordered) - len(finite),
            "median": None,
            "ci_low": None,
            "ci_high": None,
            "p": None,
            "p_holm": None,
            "flagged": None,
        }
        if len(finite) >= MIN_SAMPLES:
            low, high, p = _bootstrap(finite, null)
            row.update(median=statistics.median(finite), ci_low=low, ci_high=high)
            if index > 0:
                row["p"] = p
        rows.append(row)
    tested = [r for r in rows[1:] if r["p"] is not None]
    for row, adjusted in zip(tested, holm([r["p"] for r in tested])):
        row["p_holm"] = adjusted
        row["flagged"] = adjusted < ALPHA
    return rows


def negative_control(rows, null, n_rep=20):
    """False-flag rate of the test on data whose median is the no-change value.

    Each tested setting's values are shifted so their median equals `null`, then
    resampled (same n) n_rep times; each resample goes through the same bootstrap
    test, Holm-adjusted across settings within a resample. A calibrated test flags
    at most about 5% of these. Shuffling sample labels is not used: for a measure
    that is not at its no-change value when labels carry no signal, a shuffled run
    has a real effect and its flags are not false.
    """
    rng = random.Random(2)
    shifted = [
        [v - (row["median"] - null) for v in row["values"]]
        for row in rows[1:]
        if row["p"] is not None
    ]
    n_tested = n_flagged = 0
    for _ in range(n_rep):
        ps = [_bootstrap(rng.choices(vals, k=len(vals)), null)[2] for vals in shifted]
        adjusted = holm(ps)
        n_tested += len(adjusted)
        n_flagged += sum(a < ALPHA for a in adjusted)
    rate = n_flagged / n_tested if n_tested else None
    return {"n_rep": n_rep, "n_tested": n_tested, "n_flagged": n_flagged, "rate": rate}
