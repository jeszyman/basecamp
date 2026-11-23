"""The setting space of a study, and the scripted search the 9B is compared against.

A study's measure.py defines SPACE, a dict of parameter name -> (minimum, maximum,
step) with integer values, and CONSTRAINTS, a list of strings of the form
"<a> <op> <b>" where a and b are parameter names or integers and op is one of
<, <=, >, >= (for example "lo < hi" or "hi >= 70"). Constraints are parsed, never
evaluated as code.
"""
import operator

OPS = {"<": operator.lt, "<=": operator.le, ">": operator.gt, ">=": operator.ge}


class SpaceError(Exception):
    """A constraint string is not of the form '<a> <op> <b>'."""


def _term(token, setting):
    if token in setting:
        return setting[token]
    try:
        return int(token)
    except ValueError as err:
        raise SpaceError(f"unknown name {token!r} in constraint") from err


def _holds(constraint, setting):
    parts = constraint.split()
    if len(parts) != 3 or parts[1] not in OPS:
        raise SpaceError(f"constraint {constraint!r} is not '<a> <op> <b>'")
    return OPS[parts[1]](_term(parts[0], setting), _term(parts[2], setting))


def setting_name(setting, space):
    return "-".join(str(setting[p]) for p in space)


def in_space(setting, space, constraints=()):
    """True when the setting names exactly the space's parameters, each on its grid, and meets the constraints."""
    if set(setting) - {"name"} != set(space):
        return False
    for param, (low, high, step) in space.items():
        value = setting[param]
        if not isinstance(value, int) or isinstance(value, bool):
            return False
        if value < low or value > high or (value - low) % step:
            return False
    return all(_holds(c, setting) for c in constraints)


def _random_point(space, constraints, rng, tried, tries=1000):
    for _ in range(tries):
        point = {p: rng.randrange(low, high + 1, step) for p, (low, high, step) in space.items()}
        key = tuple(point[p] for p in space)
        if key not in tried and in_space(point, space, constraints):
            return point
    return None


def scripted_proposer(space, constraints, history, k, rng, state, better="higher"):
    """Compass search: random points until 3 results exist, then steps along one
    parameter at a time from the best result so far, halving the step when no
    step improves. `history` is a list of (setting, score); `state` holds the
    current step sizes between calls. Returns up to k new settings."""
    tried = {tuple(s[p] for p in space) for s, _ in history}
    sign = 1 if better == "higher" else -1
    proposals = []

    def add(point):
        key = tuple(point[p] for p in space)
        if key not in tried and in_space(point, space, constraints):
            tried.add(key)
            point["name"] = setting_name(point, space)
            proposals.append(point)

    scored = [(s, v) for s, v in history if v is not None]
    if len(scored) >= 3:
        if "step" not in state:
            state["step"] = {p: max(step, ((high - low) // 4) // step * step) for p, (low, high, step) in space.items()}
            state["best"] = None
        best, best_score = max(scored, key=lambda sv: sign * sv[1])
        if state["best"] is not None and sign * best_score <= sign * state["best"]:
            state["step"] = {p: max(space[p][2], (s // 2) // space[p][2] * space[p][2]) for p, s in state["step"].items()}
        state["best"] = best_score
        for param in space:
            for direction in (1, -1):
                point = {p: best[p] for p in space}
                point[param] += direction * state["step"][param]
                add(point)
    while len(proposals) < k:
        point = _random_point(space, constraints, rng, tried)
        if point is None:
            break
        add(point)
    return proposals[:k]
