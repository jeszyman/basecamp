"""Statistics harness for local-sci.

Takes a measure module (CASE_LABEL, NO_CHANGE_VALUE, SETTINGS, load, measure),
runs the measure at each setting, and for each setting reports the median over
case samples, a 95% interval of that median, a two-sided p against the no-change
value, and a Holm-adjusted p across the settings tested.

Intervals and p come from a stratified bootstrap that re-runs the measure: each
of 2,000 draws resamples the samples of each label group with replacement at full
size and calls measure on them, so any quantity the measure estimates from the
whole cohort (a healthy reference, a pooled tumor profile) is re-estimated in
every draw. A repeated sample appears as "<id>#<k>"; measure looks data up by the
part before "#". The interval is the 2.5th and 97.5th percentiles of the draw
medians, and p = 2 * min(share of draw medians <= null, share >= null), so the
interval excludes the no-change value exactly when p < 0.05.
"""
import importlib.util
import math
import random
import statistics

N_DRAWS = 2000
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


def _finite(values):
    return [v for v in values if math.isfinite(v)]


def _bootstrap(case_values_of, groups, null, seed=1):
    """Percentile interval and matching two-sided p for the median.

    case_values_of(draw_labels) returns the finite case values computed on one
    draw; groups maps each label to its sorted sample ids.
    """
    rng = random.Random(seed)
    medians = []
    for _ in range(N_DRAWS):
        draw = {}
        for label, members in groups.items():
            for k, s in enumerate(rng.choices(members, k=len(members))):
                draw[f"{s}#{k}"] = label
        vals = case_values_of(draw)
        if len(vals) >= 2:
            medians.append(statistics.median(vals))
    if len(medians) < N_DRAWS // 2:
        return None, None, None
    medians.sort()
    b = len(medians)
    low = medians[int(0.025 * b)]
    high = medians[int(0.975 * b) - 1]
    below = sum(m <= null for m in medians) / b
    above = sum(m >= null for m in medians) / b
    p = min(1.0, max(1.0 / b, 2 * min(below, above)))
    return low, high, p


def _groups(labels):
    groups = {}
    for s in sorted(labels):
        groups.setdefault(labels[s], []).append(s)
    return groups


def check_no_change(mod, data, labels, setting):
    """Stop unless each case value at the no-change setting equals NO_CHANGE_VALUE."""
    null = float(mod.NO_CHANGE_VALUE)
    values = mod.measure(data, labels, setting)
    off = [v for v in values.values() if not (math.isfinite(v) and abs(v - null) <= NULL_TOL)]
    if off:
        raise NoChangeError(f"setting {setting['name']!r} gave {len(off)} of {len(values)} values away from {null}")


def evaluate(mod, data, labels, setting, is_no_change=False):
    """One setting's row: median over case samples, bootstrap interval, and p (no Holm)."""
    null = float(mod.NO_CHANGE_VALUE)
    values = mod.measure(data, labels, setting)
    ordered = [values[s] for s in sorted(values)]
    finite = _finite(ordered)
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
        theta = statistics.median(finite)
        row["median"] = theta
        if is_no_change:
            row.update(ci_low=theta, ci_high=theta)
        else:
            low, high, p = _bootstrap(
                lambda draw: _finite(mod.measure(data, draw, setting).values()), _groups(labels), null
            )
            row.update(ci_low=low, ci_high=high, p=p)
    return row


def apply_holm(rows):
    """Holm-adjust the p of the given rows in place and set their flags."""
    tested = [r for r in rows if r["p"] is not None]
    for row, adjusted in zip(tested, holm([r["p"] for r in tested])):
        row["p_holm"] = adjusted
        row["flagged"] = adjusted < ALPHA
    return rows


def run_sweep(mod, data, labels, check_null=True):
    names = [s["name"] for s in mod.SETTINGS]
    if len(set(names)) != len(names):
        raise DuplicateSettingError(f"settings repeat a name: {names}")
    if check_null:
        check_no_change(mod, data, labels, mod.SETTINGS[0])
    rows = [evaluate(mod, data, labels, s, is_no_change=(i == 0)) for i, s in enumerate(mod.SETTINGS)]
    apply_holm(rows[1:])
    return rows


def split_samples(labels, seed):
    """Split each label group in half at random: (discovery labels, held-out labels)."""
    rng = random.Random(seed)
    discovery, holdout = {}, {}
    for label, members in _groups(labels).items():
        shuffled = list(members)
        rng.shuffle(shuffled)
        half = len(shuffled) // 2
        for s in shuffled[:half]:
            discovery[s] = label
        for s in shuffled[half:]:
            holdout[s] = label
    return discovery, holdout


def confirm(mod, data, holdout_labels, picks):
    """Rows for the picks on the held-out samples, Holm-adjusted over the picks only."""
    return apply_holm([evaluate(mod, data, holdout_labels, s) for s in picks])


def negative_control(rows, null, n_rep=20):
    """False-flag rate of the half-sample test on data whose median is the no-change value.

    Each tested setting's values are shifted so their median equals `null`; each
    replicate draws a new set of the same size from them (with replacement) and
    runs the same bootstrap test, Holm-adjusted across settings. A test that
    holds its 5% error rate flags about 5% or fewer. This checks the interval
    method at the cohort's size and spread; it does not re-run the measure.
    """
    rng = random.Random(2)
    shifted = [
        [v - (row["median"] - null) for v in row["values"]]
        for row in rows[1:]
        if row["p"] is not None
    ]
    n_tested = n_flagged = 0
    for rep in range(n_rep):
        ps = []
        for vals in shifted:
            draw = rng.choices(vals, k=len(vals))
            ids = {f"x{i}": v for i, v in enumerate(draw)}
            _, _, p = _bootstrap(
                lambda d: [ids[s.split("#")[0]] for s in d], {"case": sorted(ids)}, null, seed=rep + 1
            )
            ps.append(1.0 if p is None else p)
        adjusted = holm(ps)
        n_tested += len(adjusted)
        n_flagged += sum(a < ALPHA for a in adjusted)
    rate = n_flagged / n_tested if n_tested else None
    return {"n_rep": n_rep, "n_tested": n_tested, "n_flagged": n_flagged, "rate": rate}
